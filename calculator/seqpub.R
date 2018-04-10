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
    minus_opt1 = case_when(
      minus_tens > 0 & minus_ones != 0 ~ minus_correct - 2 * minus_ones,
      minus_tens == 0 & minus_ones > 0 ~ 10 - minus_ones,
      minus_tens > 0 & minus_ones == 0 ~ minus_correct + minus_correct %/% 10
    ),
    minus_opt2 = minus_correct + 10,
    minus_opt3 = case_when(
      minus_correct %/% 10 > 0 ~ minus_correct - 10,
      minus_correct %/% 10 == 0 & minus_ones > 0 ~ 20 - minus_ones,
      minus_correct %/% 10 == 0 & minus_ones < 0 ~ 10 - minus_correct
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
  mutate(title = paste(left, recode(type, add = "+", minus = "-"), right))
# output itembank
itembank %>%
  select(type, left, right) %>%
  write_csv(here("calculator", "itembank.csv"))

# sequence generation
n_elem <- 50
# get the assess items
assess_items <- itembank %>%
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
  unnest(seqtbl)
# randomise
set.seed(1)
run_length <- 6 * n_elem
run_order <- vector(length = run_length)
run_order[c(seq(1, run_length, 2), seq(2, run_length, 2))] <- 1:run_length
assess_seq <- assess_items %>%
  group_by(run, type) %>%
  nest() %>%
  mutate(rnd = map(data, sample_frac)) %>%
  select(-data) %>%
  unnest(rnd) %>%
  group_by(run) %>%
  nest() %>%
  mutate(
    seq_char = map(
      data,
      ~ .x[run_order, ] %>%
        select(type, left, right) %>%
        summarise_all(~ paste(., collapse = ","))
    )
  ) %>%
  select(-data) %>%
  unnest(seq_char)
jsonlite::write_json(assess_seq, here("calculator", "sequence.json"))
