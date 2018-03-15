# this is a sequence generation script used only for Filtering task
seqgen <- function(n_stim = 40, seed = n_stim) {
  # set seed for replication
  set.seed(seed)

  if (n_stim %% 40 != 0)
    stop(" Please make sure the number of stimuli is a multiple of 40.")

  # set regular sequence
  seq_regular <- data.frame(
    stim = rep(1:10, times = n_stim / 10),
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
  seq_cand
}
