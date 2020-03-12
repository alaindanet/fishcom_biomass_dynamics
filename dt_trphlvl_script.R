# Importation des packages
library (tidyverse)
library (dplyr)
library (magrittr)
#Import des fichiers
load("~/Desktop/fishcom_biomass_dynamics/trphlvl_cba.RData")

#Ajout du nb d'années
trphlvl_cba_nb <- trphlvl_cba %>% 
  group_by(station)%>%
  mutate(nb_year = year - min(year))

#Un modèle linéaire par station
bystation_trphlvl <-trphlvl_cba_nb%>%
  group_by(station)%>%
  nest() %>%
  mutate(
    model = map(data, ~ lm(w_trph_lvl_avg ~ nb_year, .x)),
    coeff = map(model, broom::tidy)
  )
#Tableau des coefficients richness
trphlvl_unnest <-unnest(bystation_trphlvl, coeff)

#Suppression des colonnes inutiles
trphlvl_coeff <- trphlvl_unnest[ , - c(2:3)]

#Suppression des parenthèses
datat<-trphlvl_coeff
datat[-1] <- lapply(datat[-1], gsub, pattern = "(Intercept)", replacement = "Intercept", fixed = TRUE)
datat[-1] <- lapply(datat[-1], gsub, pattern = "nb_year", replacement = "Pente", fixed = TRUE)
#Division en deux du data frame
datat_part1 <- dplyr::select(datat, station, term, estimate) %>%
  pivot_wider(names_from = term, values_from = estimate)

#Supprimer les lignes intercept du data_part2
datat_part2 <- dplyr::select(datat, station, term, std.error, statistic, p.value)
datat_part3 <- datat_part2[seq(2, nrow(datat_part2), 2),] %>% 
  transmute(std.error_pente = std.error, statistic_pente = statistic, p.value_pente = p.value)

#Fusionner les deux jeux de données
dt_trphlvl <- left_join(datat_part1, datat_part3, by = "station")
