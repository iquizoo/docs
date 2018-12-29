# load packages
library(tidyverse)

# function used to generate each kind of sequence
mat2char <- function(mat) {
  apply(
    mat, 2,
    function(x) paste(x, collapse = "")
  )
}
seqgen <- function(slen, seed = slen) {
  # fix the random seed
  set.seed(seed)

  # note each kind of length will do twice
  slen_full <- 2 * slen

  # symmetry sequence
  sym_seq <- mat2char(matrix(sample(rep(c("s", "n"), slen)), ncol = 2))

  # location sequence
  loc_seq <- mat2char(replicate(2, sample(sprintf("%x", 0:15), slen)))

  # seq output
  data.frame(
    slen = slen,
    sym = sym_seq,
    loc = loc_seq
  )
}

# generate all the sequences
slens <- 3:6
seq2pub <- lapply(slens, seqgen) %>%
  reduce(rbind) %>%
  sample_frac() %>%
  summarise_all(~ paste(., collapse = ","))
jsonlite::write_json(seq2pub, here::here("butterfly", "butterfly.json"))
