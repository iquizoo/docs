# clear jobs
rm(list = ls())

# load packages and sources
library(jsonlite)
library(tidyverse)
svdir <- getSrcDirectory(function(x) x)
source(file.path(svdir, "seqgen.R"))

# get all the possible sequences
seqlength <- 32 * 1:4
seeds <- seqlength
seq_cfg <- tibble(
  length = rep(seqlength, length(seeds)),
  seed = rep(seeds, each = length(seqlength))
)

# combine within each sequence
seq_res <- seq_cfg %>%
  mutate(
    res = map2(
      length, seed,
      function(.x, .y){
        as.data.frame(
          lapply(
            main(.x, .y),
            function(seq_vec)
              # join sequence with comma
              paste(seq_vec, collapse = ",")
          ),
          stringsAsFactors = FALSE
        )
      }
    )
  ) %>%
  unnest()

# combine between sequences
seq_combine <- seq_res %>%
  group_by(length) %>%
  nest(-seed, -trial) %>%
  mutate(
    res = map(
      data,
      function(.x){
        as.data.frame(
          lapply(.x, function(x) paste(x, collapse = ";")),
          stringsAsFactors = FALSE
        )
      }
    )
  ) %>%
  select(-data) %>%
  unnest(res)

# write as json
write_json(seq_combine, file.path(svdir, "seq.json"))
