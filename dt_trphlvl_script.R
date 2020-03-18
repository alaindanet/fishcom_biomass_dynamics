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

#Suppression des parenthèses: https://stackoverflow.com/a/53622690
datat <- trphlvl_coeff %>%
  mutate(term = str_replace_all(term, "[//(//)]", ""))
str(datat)

#Division en deux du data frame
datat_part1 <- dplyr::select(datat, station, term, estimate) %>%
  pivot_wider(names_from = term, values_from = estimate)

#Supprimer les lignes intercept du data_part2
datat_part2 <- dplyr::select(datat, station, term, std.error, statistic, p.value)
datat_part3 <- datat_part2[seq(2, nrow(datat_part2), 2),] %>% 
  transmute(std.error_pente = std.error, statistic_pente = statistic, p.value_pente = p.value)

#Fusionner les deux jeux de données
dt_trphlvl <- left_join(datat_part1, datat_part3, by = "station")

graphe_trphlvl <-ggplot(dt_trphlvl, aes(x=Intercept, y=nb_year, color=p.value_pente)) + geom_point()
graphe_trphlvl

#Ajout de la biomasse initiale
biomass_init <- read.table("//Users//Loubnaem//Desktop//biomass_init.txt",header=TRUE,dec=",") %>%
  mutate(station=as.character(station))
dt_troph_biomass_init <- left_join(dt_richness, biomass_init, by = "station")

graphe_troph_biomass_init <-ggplot(dt_troph_biomass_init, aes(x=Intercept, y=nb_year, color=log(biomass_init))) + geom_point()
graphe_troph_biomass_init


