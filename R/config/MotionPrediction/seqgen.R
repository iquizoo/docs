# generate sequence for motion prediction task
# number of trials
n_trials <- 80
# set random seed
set.seed(20180828)
# generate sequence of rabbit distances: random range from 500 to 1000
dist_rabbit <- round(runif(n_trials, 500, 1000))
# generate sequence of tortoise distances: random range 200 to [dist_rabbit] - 200
dist_tortoise <- round(runif(n_trials, 200, dist_rabbit - 200))
# generate sequence of winners
winner_rle <- list()
winner_rle$lengths <- as.vector(t(replicate(2, sample(rep(c(1, 2, 2, 3), n_trials / 8 / 2)))))
winner_rle$values <- rep(sample(c("Rabbit", "Tortoise")), n_trials / 2 / 2)
winner <- inverse.rle(winner_rle)
# generate sequence of durations of arrival for the winner: random range from 2.5 to 3.5
time_winner <- round(runif(n_trials, 2500, 3500), -2)
# write sequence as a json file
jsonlite::write_json(
  list(
    dist_rabbit = paste(dist_rabbit, collapse = ","),
    dist_tortoise = paste(dist_tortoise, collapse = ","),
    winner = paste(winner, collapse = ","),
    time_winner = paste(time_winner, collapse = ",")
  ),
  file.path("static/seq/08301_MotionPrediction.json"),
  auto_unbox = TRUE
)
