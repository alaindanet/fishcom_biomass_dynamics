library(tidyverse)
library(magrittr)

mypath <- rprojroot::find_package_root_file
source(mypath("R", "misc.R"))

#### Création d'un tableau avec niveau trophique moyen et années ###
myload(network_metrics, dir = mypath("data", "classes"))
myload(biomass_ts_sax, dir = mypath("data"))
    
# Troph_level_avg is not in network_metrics ? 
data_troph_level <- dplyr::select(network_metrics, opcod, troph_level_avg) %>%
  left_join(cba, data_troph_level, by = "opcod") %>%
  filter(!is.na(station) & !is.na(opcod))

ts_classification_tr <- dplyr::select(biomass_ts_sax, station, sax)
# S'assurer que la variable station soit en character dans les deux data frame
metrics_troph_lvl <- dplyr::select(data_troph_level, station, opcod, year, biomass, troph_level_avg) %>%
  mutate(station = as.character(station))

metrics_tr <- metrics_troph_lvl %>%
  # ajouter les classifications
  left_join(biomass_ts_sax, by = "station")

#### Ne garder que les cba ####
troph_level_cba <- filter(metrics_tr, sax == "cba") %>%
  dplyr::select(sax, station, opcod, year, biomass, troph_level_avg)

# Ajouter le nombre d'années
nb_troph_level <-troph_level_cba %>%
  group_by(station)%>%
  mutate(nb_year = year - min(year))

### Un modèle linéaire par station ###
by_station_troph_level<-nb_troph_level%>%
  group_by(station)%>%
  nest() %>%
  mutate(
    model = map(data, ~ try(lm(troph_level_avg ~ nb_year, .x))),
    coeff = map(model, ~ try(broom::tidy(.x)))
  )

### Tableau avec les coefficients ###
coefficients_unnest_troph_level <- unnest(by_station_troph_level, coeff)

### Retirer les colonnes inutiles ###
coefficients_troph_level <- coefficients_unnest_troph_level[ , - c(2:3)]

### Remplacer (intercept) par intercept -> Extraire les données sur Excel ###
write.table(coefficients_troph_level, file="/Users/Loubnaem/Desktop/trophlvl.xls",, quote=TRUE,
            dec=",", row.names=FALSE, col.names=TRUE, sep ="\t", qmethod = c("escape"))

# Sur excel : remplacer "(Intercept)" par "intercept"
coefficients_mixt_trophlvl <-read.table("//Users//Loubnaem//Desktop//trophlvl.txt",header=TRUE,dec=",")

### Créer une colonne pour l'intercept et une colonne pour year ###
df_troph_lvl <-coefficients_mixt_trophlvl %>%
  pivot_wider(names_from = term, values_from = estimate)

