library(tidyverse)
sample_single_stage <- function(len_vec, num_rep = 2) {
  sample(rep(len_vec, num_rep))
}
generate_sequence <- function(len_lst, num_rep = 2) {
  len_lst %>%
    map(~sample_single_stage(.x)) %>%
    unlist(use.names = FALSE)
}
set.seed(20190125)
length_candidates <- list(easy = 2:3, normal = 4:5, hard = 6:7)
seq_result <- replicate(2, generate_sequence(length_candidates)) %>%
  as_tibble(.name_repair = "minimal") %>%
  set_names(c("normal", "extra")) %>%
  gather(instance, length) %>%
  group_by(instance) %>%
  nest() %>%
  ungroup() %>%
  mutate(
    result = walk2(
      instance, data,
      ~ jsonlite::write_json(
        .y, file.path("static/seq", paste0("cheese_", .x, ".json")))
    )
  )
