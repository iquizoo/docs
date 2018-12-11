# generate sequence for stop signal task
# set random seed
set.seed(20180813)
# 4 sequences of length 32 are required
n_phase <- 4
n_stims <- 32
stims <- vector("list", n_phase)
stops <- vector("list", n_phase)
stims_regular <- rep(c("Left", "Right"), n_stims / 2)
stops_regular <- rep("Go", n_stims)
for (phase in 1:n_phase) {
  stims[[phase]] <- sample(stims_regular)
  stops_phase <- stops_regular
  is_stop_idx <- (0:(n_stims / 4 - 1)) * 4 + sample(1:4, n_stims / 4, replace = TRUE)
  stops_phase[is_stop_idx] <- "Stop"
  stops[[phase]] <- stops_phase
}
# transform sequence into a character string
stims_seq_str <- paste0(purrr::map_chr(stims, ~paste(.x, collapse = ",")), collapse = ";")
stops_seq_str <- paste0(purrr::map_chr(stops, ~paste(.x, collapse = ",")), collapse = ";")
# write out as json file
jsonlite::write_json(
  list(stim = stims_seq_str, type = stops_seq_str),
  file.path("static/seq/06301_StopSignal.json"),
  auto_unbox = TRUE
)
