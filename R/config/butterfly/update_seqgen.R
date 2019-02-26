library(tidyverse)

generate_sequence <- function(len_cands, num_rep = 2, .tail = c(7, 5, 7, 6)) {
  # add occurrence to length data in order that `setdiff` will work properly
  len_cands_full <- tibble(length = rep(len_cands, num_rep)) %>%
    group_by(length) %>%
    mutate(occur = row_number(length)) %>%
    ungroup()
  len_tail <- tibble(length = .tail) %>%
    group_by(length) %>%
    mutate(occur = row_number(length)) %>%
    ungroup()
  seq_length <- c(sample(setdiff(len_cands_full, len_tail)$length), .tail)
  symmetry <- rep(c("sym", "asym"), sum(len_cands_full$length) / 2)
  seq_symmetry <- sample(symmetry)
  locations <- 1:16
  seq_result <- tibble(length = seq_length) %>%
    # extract the corresponding symmetry sequence for each length
    mutate(
      sym_index_end = cumsum(length),
      sym_index_start = lag(sym_index_end, default = 0) + 1,
      symmetry = map2_chr(
        sym_index_start, sym_index_end,
        ~ paste(seq_symmetry[.x:.y], collapse = "-")
      ),
      location = map_chr(
        length, ~ paste(sample(locations, .x), collapse = "-")
      )
    ) %>%
    select(length, symmetry, location)
  return(seq_result)
}

set.seed(20190125)
length_candidates <- 3:7
instances <- c("normal", "extra") # extra is for testing twice shortly
seq_wrapper <- tibble(
  instance = rep(instances, each = length(length_candidates)),
  len_cands = rep(length_candidates, times = length(instances))
) %>%
  group_by(instance) %>%
  nest() %>%
  mutate(
    seq_result = map(
      data, ~ generate_sequence(.x$len_cands)
    ),
    output = walk2(
      seq_result, instance,
      ~ jsonlite::write_json(
        .x, file.path("static/seq", paste0("butterfly_", .y, ".json"))
      )
    )
  )
