# load packages
library(tidyverse)
library(jsonlite)
library(here)

# get all the possible pairs of stimuli
stims <- expand.grid(left = 10:99, right = 10:99) %>%
  mutate(
    add_correct = left + right,
    minus_correct = left - right
  ) %>%
  mutate(
    add_tens = left %/% 10 + right %/% 10,
    add_ones = left %% 10 + right %% 10,
    minus_tens = left %/% 10 - right %/% 10,
    minus_ones = left %% 10 - right %% 10,
    add_opt1 = ifelse(add_ones < 5, add_correct + 1, add_correct - 1),
    add_opt2 = add_correct + 10,
    add_opt3 = add_correct - 10,
    minus_opt1 = ifelse(
      (minus_ones >= 0 & minus_ones < 5) | (minus_ones < 0 & 10 + minus_ones < 5),
      minus_correct + 1, minus_correct - 1
    ),
    minus_opt2 = ifelse(
      (minus_ones >= 0 & minus_tens < 5) | (minus_ones < 0 & minus_tens < 6),
      minus_correct + 10, minus_correct - 10
    ),
    minus_opt3 = case_when(
      minus_ones == 0 ~ minus_correct - 10 ,
      minus_ones > 0 ~ minus_tens * 10 + 10 - minus_ones,
      minus_ones < 0 ~ minus_tens * 10 + 10 + minus_ones
    ),
    add_lvl = case_when(
      add_tens < 10 & add_ones < 10 ~ 1,
      add_tens > 10 & add_ones > 10 ~ 3,
      TRUE ~ 2
    ),
    minus_lvl = case_when(
      minus_tens == 0 | minus_ones == 0 ~ 1,
      minus_ones < 0 ~ 3,
      TRUE ~ 2
    )
  ) %>%
  gather(option, value, add_correct, minus_correct, contains("opt")) %>%
  separate(option, c("type", "opt")) %>%
  mutate(
    type = factor(type),
    level = factor(
      ifelse(type == "add", add_lvl, minus_lvl),
      levels = 1:3, labels = c("easy", "medium", "difficult")
    )
  ) %>%
  spread(opt, value) %>%
  filter(type == "add" | (type == "minus" & correct > 0)) %>%
  select(left, right, type, level, correct, opt1, opt2, opt3)
write_csv(stims, here("balloon", "stim_pool.csv"))
set.seed(1)
write_csv(sample_n(stims, 10), here("balloon", "stimuli.csv"))
