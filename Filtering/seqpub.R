# load package
library(tidyverse)

# basic settings
task_code <- "06901"
task_name <- "Filtering"
sep_inner <- ","
sep_outer <- ";"
stim_config <- read_csv(here::here("Filtering", "stimuli.csv"))

# this is a sequence generation script used only for Filtering task
seqgen <- function(n_stim = 40, seed = n_stim) {
  # set seed for replication
  set.seed(seed)

  if (n_stim %% 40 != 0)
    stop(" Please make sure the number of stimuli is a multiple of 40.")

  # set regular sequence
  seq_regular <- data.frame(
    id = rep(1:10, times = n_stim / 10),
    type = rep(c("Stay", "Stay", "Clockwise", "Counter"), each = n_stim / 4),
    cresp = rep(c("Right", "Right", "Left", "Left"), each = n_stim / 4),
    stringsAsFactors = FALSE
  )

  # randomisation
  repeat {
    rand_order <- sample.int(n_stim)
    seq_cand <- seq_regular[rand_order, ]
    cresp_rle <- rle(seq_cand$cresp)
    # ensure the same conditions do not continue more than 3 times
    if (all(cresp_rle$lengths <= 3)) break
  }

  # output
  seq_cand
}

# stimuli length settings
n_stimuli <- 40
seed <- 1:5

# generate sequence
raw_seq <- mapply(seqgen, n_stimuli, seed, SIMPLIFY = FALSE)
raw_seq_df <- tibble(
  length = n_stimuli,
  seed = 1:5,
  seq = raw_seq
)

# compose output data.frame
seq_df <- raw_seq_df %>%
  unnest(seq) %>%
  left_join(stim_config) %>%
  group_by(length, seed) %>%
  summarise_all(funs(paste(., collapse = sep_inner))) %>%
  select(length, id, target, distractor, type, cresp) %>%
  group_by(length) %>%
  summarise_all(funs(paste(., collapse = sep_outer)))

jsonlite::write_json(seq_df, here::here(task_name, paste0(task_code, "_", task_name, ".json")))
