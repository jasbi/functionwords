# Run once locally (from docs) to build small feather files for deployment.
# Requires the large contract-processed file in ../processed_data/.
#
#   cd docs
#   Rscript data_prep.R

library(feather)
library(tidyverse)
library(brms)

data_dir <- "data"
dir.create(data_dir, showWarnings = FALSE)

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

## Use the full available age range for population trajectories so website
## growth curves match `processing/englishPlotsAndCurves.Rmd` (no 12-48 restriction).
functionword_relfreq_pop <- english_tokens_contract_processed %>%
  filter(speaker == "child") %>%
  group_by(word, age) %>%
  summarise(freq = n(), .groups = "drop") %>%
  group_by(age) %>%
  mutate(
    total = sum(freq),
    relfreq = freq / total,
    ppt = relfreq * 1000
  ) %>%
  ungroup()

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

## Fit Gompertz growth curves on-the-fly using the same nonlinear brms
## specification used in `processing/englishPlotsAndCurves.Rmd`. This section
## re-fits a population-level Gompertz curve for each selected `word` using
## the pooled (child) cumulative production series. Fitting can be slow;
## adjust `iter`/`chains` below if you need faster runs for development.

## Build `functionword_relfreq` like the Rmd (cumulative ppt per speaker/word)
functionword_relfreq <- english_tokens_contract_processed %>%
  group_by(word, age, speaker) %>%
  summarise(freq = n(), .groups = "drop") %>%
  group_by(speaker, age) %>%
  mutate(total = sum(freq), relfreq = freq / total, ppt = relfreq * 1000) %>%
  group_by(word, speaker) %>%
  arrange(age) %>%
  mutate(cum_freq = cumsum(freq), cum_total = cumsum(total), cumulative_relfreq = cumsum(freq) / cumsum(total), cumulative_ppt = cumulative_relfreq * 1000) %>%
  ungroup()

gompertz_formula <- bf(
  cumulative_ppt ~ upperAsymptote * exp(-exp(((growthRate * exp(1)) / upperAsymptote) * (lag - age) + 1)),
  upperAsymptote ~ 1,
  growthRate ~ 1,
  lag ~ 1,
  nl = TRUE
)

## Small mapping of per-word priors to match the Rmd where specified; fallback
## uses conservative defaults. Extend this list as needed.
get_priors <- function(w) {
  if (w == "and") {
    c(prior(uniform(0, 25), nlpar = "upperAsymptote", lb = 0, ub = 25),
      prior(uniform(0, 5), nlpar = "growthRate", lb = 0, ub = 5),
      prior(uniform(0, 72), nlpar = "lag", lb = 0, ub = 72))
  } else if (w == "the") {
    c(prior(uniform(0, 30), nlpar = "upperAsymptote", lb = 0, ub = 30),
      prior(uniform(0, 10), nlpar = "growthRate", lb = 0, ub = 10),
      prior(uniform(0, 72), nlpar = "lag", lb = 0, ub = 72))
  } else if (w == "no") {
    c(prior(uniform(0, 40), nlpar = "upperAsymptote", lb = 0, ub = 40),
      prior(uniform(0, 5), nlpar = "growthRate", lb = 0, ub = 5),
      prior(uniform(0, 72), nlpar = "lag", lb = 0, ub = 72))
  } else if (w == "not") {
    c(prior(uniform(0, 10), nlpar = "upperAsymptote", lb = 0, ub = 10),
      prior(uniform(0, 5), nlpar = "growthRate", lb = 0, ub = 5),
      prior(uniform(0, 72), nlpar = "lag", lb = 0, ub = 72))
  } else {
    c(prior(uniform(0, 30), nlpar = "upperAsymptote", lb = 0, ub = 30),
      prior(uniform(0, 10), nlpar = "growthRate", lb = 0, ub = 10),
      prior(uniform(0, 72), nlpar = "lag", lb = 0, ub = 72))
  }
}

build_gompertz_curve_ppt <- function(word_vec, traj_tbl) {
  purrr::map_dfr(word_vec, function(w) {
    dat <- functionword_relfreq %>% dplyr::filter(.data$word == w, .data$speaker == "child")
    if (nrow(dat) < 6) {
      ag <- seq(min(traj_tbl$target_child_age, na.rm = TRUE), max(traj_tbl$target_child_age, na.rm = TRUE), length.out = 200)
      return(tibble::tibble(word = w, target_child_age = ag, fitted = NA_real_))
    }

    priors <- get_priors(w)

    fit <- tryCatch(
      brm(formula = gompertz_formula, data = dat, prior = priors, iter = 2000, chains = 4, cores = 1, control = list(adapt_delta = 0.9), silent = TRUE, refresh = 0),
      error = function(e) {
        message("brm failed for ", w, ": ", e$message)
        NULL
      }
    )

    ag <- seq(min(dat$age, na.rm = TRUE), max(dat$age, na.rm = TRUE), length.out = 200)
    if (!is.null(fit)) {
      newdata <- data.frame(age = ag)
      pred_df <- as.data.frame(fitted(fit, newdata = newdata))
      fitted_vals <- pred_df[, "Estimate"]
      return(tibble::tibble(word = w, target_child_age = ag, fitted = as.numeric(fitted_vals)))
    }

    tibble::tibble(word = w, target_child_age = ag, fitted = NA_real_)
  })
}

gompertz_fitted_ppt <- build_gompertz_curve_ppt(function_words, function_word_pop_traj_data)
gompertz_fitted_relfreq <- gompertz_fitted_ppt %>% dplyr::mutate(fitted = .data$fitted / 1000)

## Compute onset estimates from fitted brms models (fixef lag), falling back to
## `processing/age_of_production.RDS` if present.
age_of_prod_path <- "../processed_data/age_of_production.RDS"
gompertz_onset_estimates <- if (file.exists(age_of_prod_path)) {
  readRDS(age_of_prod_path) %>%
    dplyr::transmute(
      word = as.character(.data$word),
      estimate = .data$Estimate,
      q2_5 = .data$Q2.5,
      q97_5 = .data$Q97.5,
      model = "bayesian"
    )
} else {
  tibble::tibble(word = character(), estimate = double(), q2_5 = double(), q97_5 = double(), model = character())
}

missing_onset <- setdiff(function_words, gompertz_onset_estimates$word)
if (length(missing_onset) > 0) {
  # attempt to fit small brms models for missing words and extract 'lag'
  extra <- purrr::map_dfr(missing_onset, function(w) {
    dat <- functionword_relfreq %>% dplyr::filter(.data$word == w, .data$speaker == "child")
    if (nrow(dat) < 6) return(NULL)
    priors <- get_priors(w)
    fit <- tryCatch(
      brm(formula = gompertz_formula, data = dat, prior = priors, iter = 2000, chains = 4, cores = 1, control = list(adapt_delta = 0.9), silent = TRUE, refresh = 0),
      error = function(e) NULL
    )
    if (is.null(fit)) return(NULL)
    fe <- tryCatch(fixef(fit), error = function(e) NULL)
    if (is.null(fe) || !("lag" %in% rownames(fe))) return(NULL)
    tibble::tibble(word = w, estimate = fe["lag", "Estimate"], q2_5 = fe["lag", "Q2.5"], q97_5 = fe["lag", "Q97.5"], model = "bayesian")
  })
  gompertz_onset_estimates <- dplyr::bind_rows(gompertz_onset_estimates, extra)
}

age_of_prod_path <- "../processed_data/age_of_production.RDS"
gompertz_onset_estimates <- if (file.exists(age_of_prod_path)) {
  readRDS(age_of_prod_path) %>%
    dplyr::transmute(
      word = as.character(.data$word),
      estimate = .data$Estimate,
      q2_5 = .data$Q2.5,
      q97_5 = .data$Q97.5,
      model = "bayesian"
    )
} else {
  tibble::tibble(
    word = character(),
    estimate = double(),
    q2_5 = double(),
    q97_5 = double(),
    model = character()
  )
}

missing_onset <- setdiff(function_words, gompertz_onset_estimates$word)
if (length(missing_onset) > 0) {
  nls_onset <- purrr::map_dfr(missing_onset, extract_nls_onset, traj_tbl = function_word_pop_traj_data)
  gompertz_onset_estimates <- dplyr::bind_rows(gompertz_onset_estimates, nls_onset)
}

gompertz_onset_estimates <- gompertz_onset_estimates %>%
  dplyr::filter(.data$word %in% function_words) %>%
  dplyr::distinct(.data$word, .keep_all = TRUE)

write_feather(tokens_per_child, file.path(data_dir, "tokens_per_child.feather"))
write_feather(child_relfreq, file.path(data_dir, "child_relfreq.feather"))
write_feather(function_word_pop_traj_data, file.path(data_dir, "function_word_pop_traj_data.feather"))
write_feather(gompertz_fitted_relfreq, file.path(data_dir, "gompertz_fitted_relfreq.feather"))
write_feather(gompertz_onset_estimates, file.path(data_dir, "gompertz_onset_estimates.feather"))

message("Wrote precomputed tables to ", normalizePath(data_dir))
