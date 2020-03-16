# Importation des packages
library (tidyverse)
library (dplyr)
library (magrittr)
#Import des fichiers
load("~/Desktop/fishcom_biomass_dynamics/biomass_cba.RData")

#Ajout du log(biomass) et du nb d'années
logbiomass_cba<-dplyr::mutate(biomass_cba,log_biomass=log10(biomass)) %>%
  group_by(station)%>%
  mutate(nb_year = year - min(year))

#Un modèle linéaire par station
bystation_biomass <-logbiomass_cba%>%
  group_by(station)%>%
  nest() %>%
  mutate(
    model = map(data, ~ lm(log_biomass ~ nb_year, .x)),
    coeff = map(model, broom::tidy)
  )
#Tableau des coefficients
biomass_unnest <-unnest(bystation_biomass, coeff)

#Suppression des colonnes inutiles
biomass_coeff <- biomass_unnest[ , - c(2:3)]
str(biomass_coeff)
##OK
#Suppression des parenthèses
data <- mutate(biomass_coeff,
                term = str_replace_all(term, "(Intercept)", "Intercept")

data <- str_replace_all(term, "(Intercept)", "Intercept")

library(stringi)
data <- stri_replace_all_fixed(biomass_coeff$term, ")", "")

gsub("\s*\([^\)]+\)","",as.character(biomass_coeff$term))

library(stringr)
str_replace(biomass_coeff$term, "\(.*\)", "")

####
str(df)
#C'est là que les estimate deviennent des caractères...
data[-1] <- lapply(data[-1], gsub, pattern = "(Intercept)", replacement = "Intercept", fixed = TRUE)
data[-1] <- lapply(data[-1], gsub, pattern = "nb_year", replacement = "Pente", fixed = TRUE)
## Réessayer avec str_replace_all. str_replace_all pour vecteurs ? 
df <- mutate_if(data, 
                is.numeric,
                str_replace_all, pattern = "(Intercept)", replacement = "Intercept")
str(df)
# essai avec un data frame modifié à la base 
#Caractères aussi 
#Division en deux du data frame
data_part1 <- dplyr::select(data, station, term, estimate) %>%
  pivot_wider(names_from = term, values_from = estimate)
#Essai : spread
data_part1_bis <- dplyr::select(data, station, term, estimate)
data_part1_spread <- tidyr::spread(data_part1_bis, term, estimate)
str(data_part1_bis)

str(data_part1)
#Supprimer les lignes intercept du data_part2
data_part2 <- dplyr::select(data, station, term, std.error, statistic, p.value)
data_part3 <- data_part2[seq(2, nrow(data_part2), 2),] %>% 
  transmute(std.error_pente = std.error, statistic_pente = statistic, p.value_pente = p.value)

#Fusionner les deux jeux de données
dt_biomass <- left_join(data_part1, data_part3, by = "station")

#ungroup
