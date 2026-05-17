english_tokens_processed <-
  read_feather("../processed_data/english_tokens_processed.feather")

english_tokens_contract_processed <-
  read_feather("../processed_data/english_tokens_contract_processed.feather")

selected_function_words <- c("the", "and", "no", "not", "i", "you", "can", "have", "do", "it")

function_words <- intersect(selected_function_words, unique(english_tokens_contract_processed$word))

tokens_per_child <- english_tokens_contract_processed %>%
  filter(target_child_age >= 12 & target_child_age <= 48) %>%
  filter(speaker == "child") %>%
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

child_full_id <- child_relfreq %>%
  mutate(display_name = paste(target_child_name, "(", corpus_name, "|", collection_name, ")")) %>%
  distinct(collection_name, corpus_name, target_child_name, display_name)

child_list_all <- child_full_id$display_name

child_name_map <- child_full_id %>%
  select(display_name, collection_name, corpus_name, target_child_name)

top_ten_children <- tokens_per_child %>%
  head(10)

collections <- sort(unique(tokens_per_child$collection_name))

# Pooled child trajectories (growth curve + cumulative): same construction as
# processing/englishPlots.Rmd — `functionword_relfreq` chunk (group_by word × age,
# then total = sum(freq) within age so ppt is share among all function-word types
# in the contract table). Uses `age` (month bins) like the Rmd; restrict 12–48.
functionword_relfreq_pop <- english_tokens_contract_processed %>%
  filter(speaker == "child", age >= 12, age <= 48) %>%
  group_by(word, age) %>%
  summarise(freq = n(), .groups = "drop") %>%
  group_by(age) %>%
  mutate(
    total = sum(freq),
    relfreq = freq / total,
    ppt = relfreq * 1000
  ) %>%
  ungroup()

# Pooled cumulative trajectories (englishPlots.Rmd functionword_relfreq): one series
# per word across all child data; cumsum(freq)/cumsum(total) within word.
function_word_pop_traj_data <- functionword_relfreq_pop %>%
  group_by(word) %>%
  arrange(age, .by_group = TRUE) %>%
  mutate(
    cum_freq = cumsum(freq),
    cum_total = cumsum(total),
    cumulative_relfreq = cum_freq / cum_total,
    cumulative_ppt = cumulative_relfreq * 1000
  ) %>%
  ungroup() %>%
  filter(word %in% function_words) %>%
  transmute(
    word,
    target_child_age = age,
    cumulative_relfreq,
    cumulative_ppt
  )

# TEMPORARY CODE
# Gompertz mean from englishPlots.Rmd brm models:
# y ~ upperAsymptote * exp(-exp((growthRate*exp(1)/upperAsymptote)*(lag - age) + 1))
try_gompertz_nls <- function(age, y) {
  ok <- is.finite(age) & is.finite(y)
  age <- age[ok]
  y <- y[ok]
  if (length(y) < 5) {
    return(NULL)
  }
  d <- tibble::tibble(age = age, y = y)
  ymax <- max(y, na.rm = TRUE)
  starts <- list(
    list(upperAsymptote = ymax * 1.05 + 1e-5, growthRate = 2, lag = stats::median(age)),
    list(upperAsymptote = ymax + 1e-5, growthRate = 1, lag = 18),
    list(upperAsymptote = ymax * 1.2 + 1e-5, growthRate = 3, lag = 24)
  )
  for (st in starts) {
    fit <- tryCatch(
      stats::nls(
        y ~ upperAsymptote * exp(-exp((growthRate * exp(1) / upperAsymptote) * (lag - age) + 1)),
        data = d,
        start = st,
        control = stats::nls.control(maxiter = 200, warnOnly = TRUE)
      ),
      error = function(e) NULL,
      warning = function(w) NULL
    )
    if (!is.null(fit)) {
      return(fit)
    }
  }
  NULL
}

# Fit on cumulative_pPT only: nls is unstable on small-scale cumulative_relfreq;
# englishPlots.Rmd brm also uses cumulative_ppt. Relfreq curve = fitted_ppt / 1000.
build_gompertz_curve_ppt <- function(word_vec, traj_tbl) {
  purrr::map_dfr(word_vec, function(w) {
    d <- traj_tbl %>% dplyr::filter(.data$word == w)
    age <- d$target_child_age
    y <- d$cumulative_ppt
    m <- try_gompertz_nls(age, y)
    ag <- seq(min(age, na.rm = TRUE), max(age, na.rm = TRUE), length.out = 200)
    if (is.null(m)) {
      return(tibble::tibble(word = w, target_child_age = ag, fitted = NA_real_))
    }
    pred <- stats::predict(m, newdata = tibble::tibble(age = ag))
    tibble::tibble(word = w, target_child_age = ag, fitted = as.numeric(pred))
  })
}

gompertz_fitted_ppt <- build_gompertz_curve_ppt(function_words, function_word_pop_traj_data)
gompertz_fitted_relfreq <- gompertz_fitted_ppt %>%
  dplyr::mutate(fitted = .data$fitted / 1000)

