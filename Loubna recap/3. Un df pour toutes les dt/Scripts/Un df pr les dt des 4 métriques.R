#Importation des packages
library (tidyverse)
library (dplyr)

#Ajouter les noms des métriques aux coefficients
#Biomasse
dt_all_biomasses_reseau <- dplyr::select(dt_all_biomasses, station, Intercept, nb_year, std.error_pente, statistic_pente, p.value_pente) %>%
  mutate(Intercept_Biomass = Intercept, Pente_Biomass = nb_year, std.error_pente_biomass = std.error_pente, statistic_pente_biomass = statistic_pente, p.value_pente_biomass = p.value_pente) %>%
  select(station, Intercept_Biomass, Pente_Biomass, std.error_pente_biomass, statistic_pente_biomass, p.value_pente_biomass)

#Richness
dt_all_richnesses_reseau <- dplyr::select(dt_all_richnesses, station, Intercept, nb_year, std.error_pente, statistic_pente, p.value_pente) %>%
  mutate(Intercept_Richness = Intercept, Pente_Richness = nb_year, std.error_pente_richness = std.error_pente, statistic_pente_richness = statistic_pente, p.value_pente_richness = p.value_pente) %>%
  select(station, Intercept_Richness, Pente_Richness,std.error_pente_richness, statistic_pente_richness, p.value_pente_richness)

#NTM
dt_all_trphlvl_reseau <- dplyr::select(dt_all_trphlvl, station, Intercept, nb_year, std.error_pente, statistic_pente, p.value_pente) %>%
  mutate(Intercept_Trphlvl = Intercept, Pente_Trphlvl = nb_year, std.error_pente_trphlvl = std.error_pente, statistic_pente_trphlvl = statistic_pente, p.value_pente_trphlvl = p.value_pente) %>%
  select(station, Intercept_Trphlvl, Pente_Trphlvl, std.error_pente_trphlvl, statistic_pente_trphlvl, p.value_pente_trphlvl)

#Connectance
dt_all_connectance_reseau <- dplyr::select(dt_all_connectances, station, Intercept, nb_year, std.error_pente, statistic_pente, p.value_pente) %>%
  mutate(Intercept_Connectance = Intercept, Pente_Connectance = nb_year, std.error_pente_connectance = std.error_pente, statistic_pente_connectance = statistic_pente, p.value_pente_connectance = p.value_pente) %>%
  select(station, Intercept_Connectance, Pente_Connectance, std.error_pente_connectance, statistic_pente_connectance, p.value_pente_connectance)

#Un data frame pour toutes les dt (biomasse + richesse + trphlvl + connectance)
all_dt <- dt_all_biomasses_reseau %>%
  left_join(dt_all_richnesses_reseau, by = "station") %>%
  left_join(dt_all_trphlvl_reseau, by = "station") %>%
  left_join(dt_all_connectance_reseau, by = "station")

###Même chose pour les dt sans filtre (qui servent pour les relations entre métriques)
#Ajouter les noms des métriques aux coefficients

#Richness sans filtre
dt_unfiltered_richnesses_reseau <- dplyr::select(dt_unfiltered_richnesses, station, Intercept, nb_year, std.error_pente, statistic_pente, p.value_pente) %>%
  mutate(Intercept_Richness = Intercept, Pente_Richness = nb_year, std.error_pente_richness = std.error_pente, statistic_pente_richness = statistic_pente, p.value_pente_richness = p.value_pente) %>%
  select(station, Intercept_Richness, Pente_Richness,std.error_pente_richness, statistic_pente_richness, p.value_pente_richness)

#NTM sans filtre
dt_unfiltered_trphlvl_reseau <- dplyr::select(dt_unfiltered_trphlvl, station, Intercept, nb_year, std.error_pente, statistic_pente, p.value_pente) %>%
  mutate(Intercept_Trphlvl = Intercept, Pente_Trphlvl = nb_year, std.error_pente_trphlvl = std.error_pente, statistic_pente_trphlvl = statistic_pente, p.value_pente_trphlvl = p.value_pente) %>%
  select(station, Intercept_Trphlvl, Pente_Trphlvl, std.error_pente_trphlvl, statistic_pente_trphlvl, p.value_pente_trphlvl)

#Connectance sans filtre
dt_unfiltered_connectance_reseau <- dplyr::select(dt_unfiltered_connectances, station, Intercept, nb_year, std.error_pente, statistic_pente, p.value_pente) %>%
  mutate(Intercept_Connectance = Intercept, Pente_Connectance = nb_year, std.error_pente_connectance = std.error_pente, statistic_pente_connectance = statistic_pente, p.value_pente_connectance = p.value_pente) %>%
  select(station, Intercept_Connectance, Pente_Connectance, std.error_pente_connectance, statistic_pente_connectance, p.value_pente_connectance)

#Un data frame pour toutes les dt sans filtre (biomasse + richesse + trphlvl + connectance)
all_unfiltered_dt <- dt_unfiltered_richnesses_reseau %>%
  left_join(dt_unfiltered_trphlvl_reseau, by = "station") %>%
  left_join(dt_unfiltered_connectance_reseau, by = "station")
