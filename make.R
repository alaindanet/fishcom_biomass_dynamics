# This minimal drake workflow demonstrates the file structure from
# https://books.ropensci.org/drake/projects.html#code-files.
# It is not the only way to organize R scripts for drake,
# but this pattern is helpful.

library(targets)
library(tarchetypes)

tar_make()

library(tidyverse)
library(magrittr)
tar_meta(fields = warnings) %>%
  filter(!is.na(warnings))
tar_make(names = sp_semeff_perc)
