#Importation des packages
library (tidyverse)
library (dplyr)
library (magrittr)
library(cowplot)

#Ajout du numéro de l'année d'échantillonnage à tous les df 
all_biomasses_nb_bi <- add_nb_year_station(.data=all_biomasses_bi)
all_richnesses_nb <- add_nb_year_station(.data=all_richnesses)
all_connectances_nb <- add_nb_year_station(.data=all_connectances)
all_trphlvls_nb <- add_nb_year_station(.data=all_trphlvls)

###BIOMASSE

#Ajout log biomass au tableau
all_biomasses_nb_bi <- all_biomasses_nb_bi %>%
  mutate(log_biomass=log10(biomass))

#Un modèle linéaire par station
bystation_all_biomasses <-all_biomasses_nb_bi%>%
  group_by(station)%>%
  nest() %>%
  mutate(
    model = map(data, ~ lm(log_biomass ~ nb_year + offset(log(surface)), .x)),
    coeff = map(model, broom::tidy)
  )
#Tableau des coefficients richness
all_biomasses_unnest <-unnest(bystation_all_biomasses, coeff)

#Suppression des colonnes inutiles
all_biomasses_coeff <- all_biomasses_unnest[ , - c(2:3)]

#Suppression des parenthèses: https://stackoverflow.com/a/53622690
datac <- all_biomasses_coeff %>%
  mutate(term = str_replace_all(term, "[//(//)]", ""))

#Division en deux du data frame
datac_part1 <- dplyr::select(datac, station, term, estimate) %>%
  pivot_wider(names_from = term, values_from = estimate)

#Supprimer les lignes intercept du data_part2
datac_part2 <- dplyr::select(datac, station, term, std.error, statistic, p.value)
datac_part3 <- datac_part2[seq(2, nrow(datac_part2), 2),] %>% 
  transmute(std.error_pente = std.error, statistic_pente = statistic, p.value_pente = p.value)

#Fusionner les deux jeux de données
dt_all_biomasses <- left_join(datac_part1, datac_part3, by = "station")

##Plot
plot(dt_all_biomasses$Intercept,dt_all_biomasses$nb_year)

###Relation biomasse initiale // dt biomasse
#Ajouter log biomasse initiale
all_biomasses_nb_bi <- all_biomasses_nb_bi %>%
  mutate(log_initial_biomass=log10(initial_biomass))

dt_all_biomasses <- dt_all_biomasses %>%
  left_join(all_biomasses_nb_bi, by = "station")

graphe_biomass_biomass_init <-ggplot(dt_all_biomasses, aes(x=nb_year.x, y=log_initial_biomass)) + 
  geom_point() + 
  geom_smooth(method=lm)
graphe_biomass_biomass_init

M1 <- lm(nb_year.x~log_initial_biomass, data=dt_all_biomasses)
summary(M1)

######################################################################

###RICHESSE SPÉCIFIQUE 
  
#Un modèle linéaire par station
bystation_all_richnesses <-all_richnesses_nb%>%
  group_by(station)%>%
  nest() %>%
  mutate(
    model = map(data, ~ lm(richness ~ nb_year + offset(log(surface)), .x)),
    coeff = map(model, broom::tidy)
  )
#Tableau des coefficients richness
all_richnesses_unnest <-unnest(bystation_all_richnesses, coeff)

#Suppression des colonnes inutiles
all_richnesses_coeff <-all_richnesses_unnest[ , - c(2:3)]

#Suppression des parenthèses: https://stackoverflow.com/a/53622690
datac <- all_richnesses_coeff %>%
  mutate(term = str_replace_all(term, "[//(//)]", ""))

#Division en deux du data frame
datac_part1 <- dplyr::select(datac, station, term, estimate) %>%
  pivot_wider(names_from = term, values_from = estimate)

#Supprimer les lignes intercept du data_part2
datac_part2 <- dplyr::select(datac, station, term, std.error, statistic, p.value)
datac_part3 <- datac_part2[seq(2, nrow(datac_part2), 2),] %>% 
  transmute(std.error_pente = std.error, statistic_pente = statistic, p.value_pente = p.value)

#Fusionner les deux jeux de données
dt_all_richnesses <- left_join(datac_part1, datac_part3, by = "station") %>%
  dplyr::filter(p.value_pente >= 0 & p.value_pente <= 0.05)

#Même jeu de données sans les filtres
dt_unfiltered_richnesses <- left_join(datac_part1, datac_part3, by = "station")

###########################################################################
#CONNECTANCE 

#Un modèle linéaire par station
bystation_all_connectances <-all_connectances_nb%>%
  group_by(station)%>%
  nest() %>%
  mutate(
    model = map(data, ~ lm(connectance ~ nb_year + offset(log(surface)), .x)),
    coeff = map(model, broom::tidy)
  )
#Tableau des coefficients richness
all_connectances_unnest <-unnest(bystation_all_connectances, coeff)

#Suppression des colonnes inutiles
all_connectances_coeff <-all_connectances_unnest[ , - c(2:3)]

#Suppression des parenthèses: https://stackoverflow.com/a/53622690
datac <- all_connectances_coeff %>%
  mutate(term = str_replace_all(term, "[//(//)]", ""))

#Division en deux du data frame
datac_part1 <- dplyr::select(datac, station, term, estimate) %>%
  pivot_wider(names_from = term, values_from = estimate)

#Supprimer les lignes intercept du data_part2
datac_part2 <- dplyr::select(datac, station, term, std.error, statistic, p.value)
datac_part3 <- datac_part2[seq(2, nrow(datac_part2), 2),] %>% 
  transmute(std.error_pente = std.error, statistic_pente = statistic, p.value_pente = p.value)

#Fusionner les deux jeux de données
dt_all_connectances <- left_join(datac_part1, datac_part3, by = "station") %>%
  dplyr::filter(p.value_pente >= 0 & p.value_pente <= 0.05)

#Même jeu de données sans les filtres
dt_unfiltered_connectances <- left_join(datac_part1, datac_part3, by = "station")

###########################################################################
#NIVEAU TROPHIQUE MOYEN

#Un modèle linéaire par station
bystation_all_trphlvl <-all_trphlvls_nb%>%
  group_by(station)%>%
  nest() %>%
  mutate(
    model = map(data, ~ lm(w_trph_lvl_avg ~ nb_year + offset(log(surface)), .x)),
    coeff = map(model, broom::tidy)
  )
#Tableau des coefficients richness
all_trphlvl_unnest <-unnest(bystation_all_trphlvl, coeff)

#Suppression des colonnes inutiles
all_trphlvl_coeff <-all_trphlvl_unnest[ , - c(2:3)]

#Suppression des parenthèses: https://stackoverflow.com/a/53622690
datac <- all_trphlvl_coeff %>%
  mutate(term = str_replace_all(term, "[//(//)]", ""))

#Division en deux du data frame
datac_part1 <- dplyr::select(datac, station, term, estimate) %>%
  pivot_wider(names_from = term, values_from = estimate)

#Supprimer les lignes intercept du data_part2
datac_part2 <- dplyr::select(datac, station, term, std.error, statistic, p.value)
datac_part3 <- datac_part2[seq(2, nrow(datac_part2), 2),] %>% 
  transmute(std.error_pente = std.error, statistic_pente = statistic, p.value_pente = p.value)

#Fusionner les deux jeux de données
dt_all_trphlvl <- left_join(datac_part1, datac_part3, by = "station") %>%
  dplyr::filter(p.value_pente >= 0 & p.value_pente <= 0.05)

#Même jeu de données sans les filtres
dt_unfiltered_trphlvl <- left_join(datac_part1, datac_part3, by = "station")

