library(targets)
library(tidyverse)
library(magrittr)
library(lubridate)
library(cowplot)
library(viridis)
library(arrow)
library(glmmTMB)
library(piecewiseSEM)
library(semEff)
library(lme4)
library(INLA)
lapply(list.files(here::here("R"), full.names = TRUE), source)

library(sf)
tar_load(c(basin_dce, station_analysis))

tar_load(full_data2)

library(igraph)

