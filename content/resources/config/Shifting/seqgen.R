# get useful helper functions
config_dir <- getSrcDirectory(function(x) x)
source(file.path(dirname(config_dir), "utils.R"))

main <- function(n_trial = 128, seed = n_trial){
  # input is number of trials (filler not included)
  # must be a multiple of 32 (8 condition * 4 stimuli per condition)

  # for replication considerations, set the seed
  set.seed(seed)

  # three types of run length are used, i.e., 1, 2, 3
  # the ratio of three types of run length is 1:2:1
  rl_seed <- c(1, 2, 2, 3)
  # tasks
  task_names <- c("Habituation", "Weight")
  # conditions
  cond_names <- c("Repeat", "Switch")
  # correct responses
  cresp_names <- c("Left", "Right")

  # set the task, condition, and correct response sequence simultanenously
  repeat {
    # get the condition sequence
    cond_seq <- rle2seq(rl_seed, cond_names, n_trial)
    # get the correct response sequence
    cresp_seq <- rle2seq(rl_seed, cresp_names, n_trial)
    # generate task sequence
    task_seq <- character(n_trial + 1)
    # set the first random filler task
    task_seq[1] <- sample(task_names, 1)
    # fill out the task sequence
    for (i_seq in 1:n_trial){
      if (cond_seq[i_seq] == "Repeat")
        task_seq[i_seq + 1] = task_seq[i_seq]
      else
        task_seq[i_seq + 1] = setdiff(task_names, task_seq[i_seq])
    }

    # check the validity of sequence
    task_seq_interest <- task_seq[-1]
    # for the combined conditions (total: 2^3 = 8), each has equal occurrences
    occurrences <- summary(factor(paste(task_seq_interest, cond_seq, cresp_seq)))
    if (all(occurrences == n_trial / 8))
      break
  }

  # read in all the properties of stimuli
  stim <- read.csv(file.path(config_dir, "stimuli-seq.csv"), stringsAsFactors = FALSE)
  # preallocate stimuli sequence
  stim_seq <- rep(NA_integer_, n_trial)
  # get the unique combined conditions
  com_seq <- data.frame(task = task_seq_interest, cond = cond_seq, cresp = cresp_seq)
  cconds <- unique(com_seq)
  # iteration to get stimuli sequence
  for (icond in 1:nrow(cconds)){
    ccond <- cconds[icond, ]
    ccond_loc <- apply(com_seq, 1, function (x) all(x == ccond))
    repeat {
      stim_seq_tmp <- stim_seq
      # generate condidate stimulus sequence with ramdom sampling
      stim_cands <- sample(
        # repeat each stimuli with enough times by heuristic knowledge
        rep(
          # choose the right stimuli
          subset(stim, subset = task == ccond$task & cresp == ccond$cresp,
                 select = id, drop = TRUE),
          # 8 combined conditions, 4 stimuli for each condition
          n_trial / 8 / 4
        )
      )
      stim_seq_tmp[ccond_loc] <- stim_cands
      repchk <- c(diff(stim_seq_tmp), diff(stim_seq_tmp, 2))
      if (all(is.na(repchk)) || is.na(any(repchk == 0)) || !any(repchk == 0))
        break
    }
    stim_seq[ccond_loc] <- stim_cands
  }

  # choose the first trial stimulus (id and cresp)
  repeat{
    filler <- sample(setdiff(unique(stim$id), stim_seq[1:2]), 1)
    filler_cresp <- stim$cresp[stim$id == filler & stim$task == task_seq[1]]
    if (filler_cresp != cresp_seq[1])
      break
  }
  # store the results to a data.frame
  data.frame(
    trial = 1:(n_trial + 1),
    stim = c(filler, stim_seq),
    task = task_seq,
    cond = c('Filler', cond_seq),
    cresp = c(filler_cresp, cresp_seq)
  )
}
