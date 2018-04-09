# load packages
library(tidyverse)
library(jsonlite)
library(here)

# set the item bank, correct options and interference options included
itembank <- expand.grid(left = 10:99, right = 10:99) %>%
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
    difficulty = ifelse(type == "add", add_lvl, minus_lvl)
  ) %>%
  spread(opt, value) %>%
  filter(type == "add" | (type == "minus" & correct > 0)) %>%
  mutate(title = paste(left, recode(type, add = "+", minus = "-"), right)) %>%
  select(title, type, difficulty, correct, opt1, opt2, opt3)
write_csv(itembank, here("calculator", "itembank.csv"))
set.seed(1)
write_csv(sample_n(itembank, 10), here("calculator", "itembank_sample.csv"))

# sequence generation
n_elem <- 50
seq_assess <- itembank %>%
  group_by(type, difficulty) %>%
  nest() %>%
  mutate(
    R1 = map(data, ~ sample_n(.x, n_elem)),
    R2 = map2(data, R1, ~ sample_n(setdiff(.x, .y), n_elem)),
    tmp = map2(R1, R2, union),
    R3 = map2(data, tmp, ~ sample_n(setdiff(.x, .y), n_elem))
  ) %>%
  select(-data, -tmp) %>%
  gather(spl_rnd, seqtbl, R1:R3) %>%
  mutate(
    run = case_when(
      spl_rnd == "R1" & type == "minus" ~ "R2",
      spl_rnd == "R2" & type == "add" ~"R1",
      TRUE ~ spl_rnd
    )
  ) %>%
  select(run, type, difficulty, seqtbl) %>%
  arrange(run) %>%
  unnest(seqtbl) %>%
  group_by(run) %>%
  nest() %>%
  mutate(rnd = map(data, sample_frac)) %>%
  select(-data) %>%
  unnest(rnd)
write_csv(seq_assess, here("calculator", "sequence.csv"))
