# generate sequences according to run length requirement
rle2seq <- function(rl_seed, valnames, len_seq){
  # set regular run length sequence
  n_type <- length(valnames)
  reptimes <- len_seq / (sum(rl_seed) * n_type)
  stopifnot(abs(reptimes - round(reptimes)) < .Machine$double.eps ^ 0.5)
  rl_reg <- rep(rl_seed, reptimes)
  # get run length sequence
  rl_seq <- as.vector(t(replicate(n_type, sample(rl_reg))))
  # get values sequence
  val_seq <- rep(sample(valnames), reptimes * length(rl_seed))
  # inverse run length encoding
  x <- list(lengths = rl_seq, values = val_seq)
  inverse.rle(x)
}

# generate trial property sequence according to "repeat"-"switch" pattern
prop_seqgen <- function(rl_seed, propnames, len_seq){
  # get the "repeat"-"switch" pattern
  cond_names <- c("Repeat", "Switch")
  cond_seq <- rle2seq(rl_seed, cond_names, len_seq)

  # preallocate property sequence
  prop_seq <- character(len_seq + 1)
  # set the first random filler property
  prop_seq[1] <- sample(propnames, 1)
  # fill out trial property sequence
  for (i_seq in 1:len_seq){
    if (cond_seq[i_seq] == "Repeat")
      prop_seq[i_seq + 1] = prop_seq[i_seq]
    else
      prop_seq[i_seq + 1] = setdiff(propnames, prop_seq[i_seq])
  }
  prop_seq
}
