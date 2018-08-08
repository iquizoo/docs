# basic settings
set.seed(0)
stimlens <- c(5, 7, 9)
ncond <- length(stimlens)
ntrl <- 15
stims <- matrix(character(ntrl), nrow = ncond)

# begin generate
rep_each <- ntrl / ncond
for (icond in 1:ncond){
  stimlen <- stimlens[icond]
  for (i_trl in 1:rep_each){
    stim <- numeric(stimlen)
    stim[1:4] <- sample(0:9, 4)
    for (i_stim in 5:stimlen){
      repeat{
        stim_cand <- sample(0:9, 1)
        if (! stim_cand %in% stim[(i_stim - 3):(i_stim - 1)])
          break
      }
      stim[i_stim] <- stim_cand
    }
    stims[icond, i_trl] <- paste(stim, collapse = "")
  }
}

# randomise trial order
prac_stims <- stims[1:3]
test_stims <- stims[sample(4:15)]
stims_seq <- data.frame(
  prac = paste(prac_stims, collapse = ","),
  test = paste(test_stims, collapse = ",")
)
jsonlite::write_json(stims_seq, here::here("RunningMemory", "05201_RunningMemory.json"))
