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
  summarise_all(funs(paste0(., collapse = ",")))
tst_list <- stims %>%
  mutate(ver = recode(type, lure = "b", .default = "a")) %>%
  sample_frac() %>%
  select(nid, ver, sim, type) %>%
  rename(id = nid) %>%
  summarise_all(funs(paste0(., collapse = ",")))
jsonlite::write_json(lrn_list, here::here("pictureMemory", "pictureMemory.lrn.json"))
jsonlite::write_json(tst_list, here::here("pictureMemory", "pictureMemory.tst.json"))
