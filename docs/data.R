library(arrow)

word_model_db_path <- "../processed_data/word_model_db.RDS"
if (!file.exists(word_model_db_path)) {
  stop("Missing ", word_model_db_path, ". Run processing/englishPlotsAndCurves_refactored.Rmd first.")
}

word_model_db <- readRDS(word_model_db_path)
function_words <- sort(unique(as.character(word_model_db$word)))
if (length(function_words) == 0) {
  stop("No function words found in ", word_model_db_path)
}

processed_tokens_path <- "../processed_data/english_tokens_contract_processed.feather"
if (!file.exists(processed_tokens_path)) {
  stop("Missing ", processed_tokens_path, ".")
}
english_tokens_contract_processed <- read_feather(processed_tokens_path)

tokens_per_child <- english_tokens_contract_processed %>%
  filter(speaker == "child", target_child_age >= 12, target_child_age <= 48) %>%
  group_by(collection_name, corpus_name, target_child_name) %>%
  summarise(token_count = n(), .groups = "drop") %>%
  arrange(desc(token_count))

all_child_age_totals <- english_tokens_contract_processed %>%
  filter(speaker == "child", target_child_age >= 12, target_child_age <= 48) %>%
  group_by(collection_name, corpus_name, target_child_name, target_child_age) %>%
  summarise(child_total_tokens = n(), .groups = "drop")

child_relfreq <- english_tokens_contract_processed %>%
  filter(speaker == "child", word %in% function_words, target_child_age >= 12, target_child_age <= 48) %>%
  group_by(collection_name, corpus_name, target_child_name, word, target_child_age) %>%
  summarise(freq = n(), .groups = "drop") %>%
  left_join(all_child_age_totals, by = c("collection_name", "corpus_name", "target_child_name", "target_child_age")) %>%
  mutate(
    relfreq = freq / child_total_tokens,
    ppt = relfreq * 1000
  ) %>%
  group_by(collection_name, corpus_name, target_child_name, word) %>%
  arrange(target_child_age, .by_group = TRUE) %>%
  mutate(
    cum_freq = cumsum(freq),
    cum_total = cumsum(child_total_tokens),
    cumulative_relfreq = cum_freq / cum_total,
    cumulative_ppt = cumulative_relfreq * 1000
  ) %>%
  ungroup()

word_growth_observed <- word_model_db %>%
  select(word, child_data) %>%
  unnest(child_data, names_sep = "_") %>%
  transmute(
    word = as.character(word),
    age = .data$child_data_age,
    cumulative_ppt = .data$child_data_cumulative_ppt
  )

word_growth_fitted <- word_model_db %>%
  select(word, fitted_curve) %>%
  unnest(fitted_curve, names_sep = "_") %>%
  transmute(
    word = as.character(word),
    age = .data$fitted_curve_age,
    fitted_ppt = .data$fitted_curve_fitted_ppt
  )

word_growth_aop <- word_model_db %>%
  transmute(
    word = as.character(word),
    aop_estimate = .data$aop_estimate,
    aop_err = .data$aop_err,
    aop_q2_5 = .data$aop_q2_5,
    aop_q97_5 = .data$aop_q97_5
  )

child_full_id <- child_relfreq %>%
  mutate(display_name = paste0(target_child_name, " (", collection_name, " | ", corpus_name, ")")) %>%
  distinct(collection_name, corpus_name, target_child_name, display_name)

child_full_id <- child_full_id %>% arrange(collection_name, corpus_name, target_child_name)
child_list_all <- child_full_id$display_name

child_name_map <- child_full_id %>%
  select(display_name, collection_name, corpus_name, target_child_name)

top_ten_children <- tokens_per_child %>%
  head(10)

collections <- sort(unique(tokens_per_child$collection_name))

word_growth_obs_by_word <- split(word_growth_observed, word_growth_observed$word)
word_growth_fit_by_word <- split(word_growth_fitted, word_growth_fitted$word)

growth_estimate_text <- setNames(
  lapply(function_words, function(w) {
    row <- word_growth_aop %>% filter(.data$word == w)
    if (nrow(row) == 0 || !is.finite(row$aop_estimate[1])) {
      return(NULL)
    }
    ci_low <- row$aop_q2_5[1]
    ci_high <- row$aop_q97_5[1]
    ci_text <- if (is.finite(ci_low) && is.finite(ci_high)) {
      sprintf("CI = [%.2f, %.2f]", ci_low, ci_high)
    } else {
      "CI unavailable"
    }
    sprintf(
      "A bayesian growth curve model estimated the onset age of production for \"%s\" as %.1f months (%s).",
      w,
      row$aop_estimate[1],
      ci_text
    )
  }),
  function_words
)
