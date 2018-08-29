# generate sequence for weather predicting task
# set random seed
set.seed(20180824)
# set number of trials as 200
n_trial <- 200
# load probability table
probs <- readr::read_csv(file.path("content/resources/config/WxPredict/prob_table.csv"))
# use multinomial distribution to get the stimuli sequence
stim_seq <- numeric(n_trial)
outcome_seq <- character(n_trial)
last_stim <- 0
for (i_trial in 1:n_trial) {
  repeat {
    stim_candidate <- which(rmultinom(1, size = 1, prob = probs$freq_occur) == 1)
    if (stim_candidate != last_stim) {
      stim_seq[i_trial] <- stim_candidate
      last_stim <- stim_candidate
      outcome_simulation <- runif(1)
      if (outcome_simulation < with(probs, freq_rain[ID == stim_candidate])) {
        outcome_seq[i_trial] <- "Rain"
      } else {
        outcome_seq[i_trial] <- "Sunshine"
      }
      break
    }
  }
}
# write out sequence as a json file
jsonlite::write_json(
  list(
    stim = paste(stim_seq, collapse = ","),
    outcome = paste(outcome_seq, collapse = ",")
  ),
  file.path("static/seq/08201_WxPredict.json"),
  auto_unbox = TRUE
)
