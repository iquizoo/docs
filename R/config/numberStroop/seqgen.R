# load packages
library(tidyverse)

# regular sequence
regsep <- data.frame(
  id = 1:16,
  nleft = rep(c(2:5, 6:9), 2),
  nright = rep(c(6:9, 2:5), 2),
  sleft = rep(c("Big", "Small"), each = 8),
  sright = rep(c("Small", "Big"), each = 8),
  type = c(
    rep(c("Incongruent", "Congruent"), each = 4),
    rep(c("Congruent", "Incongruent"), each = 4)
  ),
  cresp = rep(c("Left", "Right"), each = 8),
  stringsAsFactors = FALSE
)

# sequence generation function
seqgen <- function(n_trial, seed = n_trial) {
  # fix random seed
  set.seed(seed)
  # #trials must be a multiple of #rows of regular sequence
  mult <- n_trial %/% nrow(regsep)
  stopifnot(abs(mult - round(mult)) < .Machine$double.eps)
  # expand regular sequence
  regsep_expand <- regsep %>%
    slice(rep(row_number(), mult))
  # randomisation and check: no more than 3 repeats of reponse property
  repeat {
    candidate <- sample_frac(regsep_expand)
    chk_id <- all(rle(candidate$id)$lengths < 3)
    chk_cresp <- all(rle(candidate$cresp)$lengths < 3)
    if (chk_id && chk_cresp)
      break
  }
  candidate %>%
    select(-id)
}

# configurations
work_dir <- here::here("numberStroop")
n_trial <- 80

# publish sequence
seq2pub <- seqgen(n_trial) %>%
  add_column(length = n_trial, .before = "nleft")
seq4js <- seq2pub %>%
  group_by(length) %>%
  summarise_all(~ paste(., collapse = ","))
jsonlite::write_json(seq4js, file.path(work_dir, "02901_numberStroop.json"))
