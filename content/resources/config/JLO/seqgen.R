# generate sequence file for `JLO` task
candidate_rotations <- 6:9
candidate_directions <- c("Left", "Right")
candidate_lengths <- 0.25 * (1:4)

candidate_configs <- expand.grid(
  rotation = candidate_rotations,
  direction = candidate_directions,
  length = candidate_lengths
)

# generate random sequence
set.seed(20180914)
random_config <- candidate_configs[sample(nrow(candidate_configs)),]

# compose final sequence and write it out
seq_string <- purrr::map(
  random_config,
  ~ paste(.x, collapse = ",")
)
jsonlite::write_json(
  seq_string,
  file.path("static/seq/08401_JLO.json"),
  auto_unbox = TRUE
)
