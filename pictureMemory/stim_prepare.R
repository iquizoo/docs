# load packages
library(tidyverse)

# load stimuli pool
stims <- read_csv(here::here("pictureMemory", "stimuli.csv"))
n_each_sim = 8
set.seed(n_each_sim)
# random selection of stimuli
stim_selection <- stims %>%
  group_by(sim) %>%
  nest() %>%
  mutate(sel = map(data, ~ sample_n(.x, n_each_sim * 2))) %>%
  mutate(
    target = map(sel, ~ sample_n(.x, n_each_sim)),
    lure = map2(sel, target, setdiff),
    foil = map2(data, sel, ~ sample_n(setdiff(.x, .y), n_each_sim))
  ) %>%
  select(-data, -sel) %>%
  gather(type, stim, target:foil) %>%
  unnest(stim) %>%
  arrange(desc(type), sim)
# rename pictures
stim_rename <- stim_selection %>%
  mutate(nid = row_number())
orig_dir <- here::here("pictureMemory", "pictureMemory")
dest_dir <- here::here("pictureMemory", "renamed")
orig_files <- file.path(orig_dir, c(sprintf("%03da.jpg", stim_rename$id), sprintf("%03db.jpg", stim_rename$id)))
dest_files <- file.path(dest_dir, c(sprintf("%03da.jpg", stim_rename$nid), sprintf("%03db.jpg", stim_rename$nid)))
if (dir.exists(dest_dir))
  (unlink(dest_dir, recursive = TRUE, force = TRUE))
dir.create(dest_dir)
file.copy(orig_files, dest_files)
write_csv(stim_rename, here::here("pictureMemory", "stim_rename.csv"))
