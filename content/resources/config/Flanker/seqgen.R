# get useful helper functions
config_dir <- getSrcDirectory(function(x) x)
source(file.path(dirname(config_dir), "utils.R"))

main <- function(n_trial = 80, seed = n_trial){
  # n_trial must be a multiple of 8 (2 conditions * 4 stimuli)

  # set random seed
  set.seed(seed)

  # read the stimuli properties table
  stimuli <- read.csv(file.path(config_dir, "stimuli-seq.csv"))

  # three types of run length are used, i.e., 1, 2
  # the ratio of three types of run length is 2:1
  rl_seed <- c(1, 1, 2)
  type_names <- c("Congruent", "Incongruent")
  cresp_names <- c("Left", "Right")

  # get the proper type and correct response sequences
  repeat {
    # generate type sequence
    type_seq <- prop_seqgen(rl_seed, type_names, n_trial)[-1]
    # generate correct response sequence
    cresp_seq <- prop_seqgen(rl_seed, cresp_names, n_trial)[-1]
    # check if all types have equal probability of different correct responses
    occurrences <- summary(factor(paste(type_seq, cresp_seq)))
    if (all(occurrences == n_trial / (length(type_names) * length(cresp_names))))
      break
  }

  # get the stimuli sequence
  idx_seq <- mapply(function(type, target)
    which(is.element(stimuli$type, type) & is.element(stimuli$target, target)),
    type_seq, cresp_seq
  )
  stimuli[idx_seq, ]
}
