# load packages
library(tidyverse)

# set all the possible stimuli characteristics
colors <- c("红色", "绿色", "蓝色", "黄色")
shapes <- c("圆形", "星形", "方形", "三角形")
numbers <- 1:4

# generate all the possible stimuli composite
stimuli <- expand.grid(颜色 = colors, 形状 = shapes, 数目 = numbers) %>%
  mutate(
    类型 = case_when(
      颜色 == "红色" & 形状 == "圆形" & 数目 == 1 ~ "备选",
      颜色 == "绿色" & 形状 == "星形" & 数目 == 2 ~ "备选",
      颜色 == "蓝色" & 形状 == "方形" & 数目 == 3 ~ "备选",
      颜色 == "黄色" & 形状 == "三角形" & 数目 == 4 ~ "备选",
      TRUE ~ "测试"
    )
  )
# separate cards for option and test
stim_opts <- stimuli %>%
  filter(类型 == "备选") %>%
  select(-类型) %>%
  add_column(编号 = LETTERS[1:4], .before = 1)
stim_test <- stimuli %>%
  filter(类型 == "测试") %>%
  select(-类型)
# remove test cards which have more than one feature in common with options
stim_test_clear <- stim_test %>%
  mutate(
    col_mirror = stim_opts$编号[match(颜色, stim_opts$颜色)],
    shp_mirror = stim_opts$编号[match(形状, stim_opts$形状)],
    num_mirror = stim_opts$编号[match(数目, stim_opts$数目)]
  ) %>%
  gather(mirror_type, mirror_id, col_mirror:num_mirror) %>%
  group_by(颜色, 形状, 数目) %>%
  summarise(
    A = sum(mirror_id == "A"),
    B = sum(mirror_id == "B"),
    C = sum(mirror_id == "C"),
    D = sum(mirror_id == "D")
  ) %>%
  ungroup() %>%
  group_by(颜色, 形状, 数目) %>%
  gather(id, n, A:D) %>%
  summarise(remain = all(n < 2)) %>%
  ungroup() %>%
  filter(remain) %>%
  select(-remain) %>%
  add_column(编号 = 1:nrow(.), .before = 1)
# output as excel csv files
write_excel_csv(stim_opts, here::here("WCST", "stim_opts.csv"))
write_excel_csv(stim_test_clear, here::here("WCST", "stim_test.csv"))

# generate sequence file
set.seed(100) # set seed for replication
seq <- replicate(2, sample_frac(stim_test_clear), simplify = FALSE) %>%
  reduce(rbind) %>%
  summarise(id = paste0(编号, collapse = ","))
# output as a json file
jsonlite::write_json(seq, here::here("WCST", "05501_WCST.json"))
