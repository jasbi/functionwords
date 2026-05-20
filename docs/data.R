library(feather)

selected_function_words <- c("the", "and", "no", "not", "i", "you", "can", "have", "do", "it")

data_dir <- "data"

tokens_per_child <- read_feather(file.path(data_dir, "tokens_per_child.feather"))
child_relfreq <- read_feather(file.path(data_dir, "child_relfreq.feather"))
function_word_pop_traj_data <- read_feather(file.path(data_dir, "function_word_pop_traj_data.feather"))
gompertz_fitted_relfreq <- read_feather(file.path(data_dir, "gompertz_fitted_relfreq.feather"))
gompertz_onset_estimates <- read_feather(file.path(data_dir, "gompertz_onset_estimates.feather")) %>%
  mutate(word = as.character(word))

function_words <- intersect(selected_function_words, unique(child_relfreq$word))

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

age_months_min <- min(
  c(child_relfreq$target_child_age, function_word_pop_traj_data$target_child_age),
  na.rm = TRUE
)
age_months_max <- max(
  c(child_relfreq$target_child_age, function_word_pop_traj_data$target_child_age),
  na.rm = TRUE
)
