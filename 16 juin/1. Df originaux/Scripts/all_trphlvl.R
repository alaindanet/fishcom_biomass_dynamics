##Data frame niveaux trophiques moyens totaux

#Importation des packages
library(dplyr)
library(magrittr)
library(tidyverse)

#Importation des fichiers
load("~/Desktop/Stage M2 CESCO/fishcom/data/biomass_ts_sax.rda")
load("~/Desktop/Stage M2 CESCO/fishcom/data/community_metrics.rda")
load("~/Desktop/Stage M2 CESCO/fishcom/data/op_analysis.rda")
load("~/Desktop/fishcom_biomass_dynamics/data/classes/network_metrics.rda")

### Data frame : all_richnesses (opcod, station, ntm, year)
op <- dplyr::select(op_analysis, station, opcod, year, surface)
net <- dplyr::select(network_metrics, opcod, w_trph_lvl_avg)
op_net <- left_join(net, op, by = "opcod") %>%
  filter(!is.na(station) & !is.na(opcod))

# Ajout de la classification cba
classification <- dplyr::select(biomass_ts_sax, station, sax)
# S'assurer que la variable station soit en character dans les deux data frame
op_station <- dplyr::select(op_analysis, station, opcod, year, surface) %>%
  mutate(station = as.character(station))

net_class <- net %>%
  # ajouter les stations à community analysis
  left_join(op_station, by = "opcod") %>%
  # ajouter les classifications
  left_join(classification, by = "station")
# Ne garder que les cba
trphlvl_cba <- filter(net_class, sax == "cba") %>%
  dplyr::select(opcod, station, w_trph_lvl_avg, year, sax, surface)
# Ne garder que les abc
trphlvl_abc <- filter(net_class, sax == "abc") %>%
  dplyr::select(opcod, station, w_trph_lvl_avg, year, sax, surface)
# Ne garder que les bbb
trphlvl_bbb <- filter(net_class, sax == "bbb") %>%
  dplyr::select(opcod, station, w_trph_lvl_avg, year, sax, surface)

all_trphlvls <- bind_rows(trphlvl_abc, trphlvl_bbb, trphlvl_cba)

