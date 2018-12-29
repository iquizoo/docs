# load libraries
library(tidyverse)

# load settings
stims <- read_csv(here::here("pictureMemory", "stim_rename.csv"))

# fix random seed
set.seed(1)

# learn list and test list
lrn_list <- stims %>%
  filter(type %in% c("target", "lure")) %>%
  mutate(ver = "a") %>%
  sample_frac() %>%
  select(nid, ver, sim) %>%
  rename(id = nid) %>%
  summarise_all(funs(paste0(., collapse = ","))) %>%
  add_column(phase = "Learn", .before = 1)
tst_list <- stims %>%
  mutate(
    ver = recode(type, lure = "b", .default = "a"),
    cresp = recode(type, target = "old", lure = "similar", foil = "new")
  ) %>%
  sample_frac() %>%
  select(nid, ver, sim, type, cresp) %>%
  rename(id = nid) %>%
  summarise_all(funs(paste0(., collapse = ","))) %>%
  add_column(phase = "Test", .before = 1)
seq_list <- list(as.list(lrn_list), as.list(tst_list))
jsonlite::write_json(
  seq_list,
  here::here("pictureMemory", "03501_pictureMemory.json"),
  auto_unbox = TRUE
)
