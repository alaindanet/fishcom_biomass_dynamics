#Importation des packages
library (tidyverse)
library (dplyr)
library(cowplot)
library(cars)
library(broom)
library(magrittr)

#Calcul de w
all_dt_poids <- all_dt %>%
  mutate(poids=abs(Pente_Biomass/std.error_pente_biomass))

#RICHNESS ~ BIOMASS 
mylm <- lm(Pente_Richness ~ Pente_Biomass, weights = poids, data = all_dt_poids)
data_to_plot <- augment(mylm) 

ggplot(data_to_plot, aes(y = Pente_Richness, x = Pente_Biomass)) +
  geom_point() +
  geom_line(aes(y = .fitted, x = Pente_Biomass), color = "blue") +
  geom_ribbon(aes(
    ymin  = .fitted - .se.fit,
    ymax  = .fitted + .se.fit
  ),
  fill = "blue", alpha = .1
  ) + xlab("Pentes biomasse") + ylab("Pentes richesse spécifique") + 
  geom_vline(xintercept = 0, color="grey") +
  geom_hline(yintercept = 0, color="grey") 

summary(mylm)
#R carré= 0,36. Intercept = 0,098. Pente = 3,62.

#CONNECTANCE ~ BIOMASS
mylm_bis <- lm(Pente_Connectance ~ Pente_Biomass, weights = poids, data = all_dt_poids)
data_to_plot_bis <- augment(mylm_bis) 

ggplot(data_to_plot_bis, aes(y = Pente_Connectance, x = Pente_Biomass)) +
  geom_point() +
  geom_line(aes(y = .fitted, x = Pente_Biomass), color = "blue") +
  geom_ribbon(aes(
    ymin  = .fitted - .se.fit,
    ymax  = .fitted + .se.fit
  ),
  fill = "blue", alpha = .1
  ) + xlab("Pentes biomasse") + ylab("Pentes connectance") + 
  geom_vline(xintercept = 0, color="grey") + 
  geom_hline(yintercept = 0, color="grey")

summary(mylm_bis)
#R carré= 0,45. Intercept = -0,0001. Pente = 0,46.

#NTM ~ BIOMASS
mylm_ter <- lm(Pente_Trphlvl ~ Pente_Biomass, weights = poids, data = all_dt_poids)
data_to_plot_ter <- augment(mylm_ter) 

ggplot(data_to_plot_ter, aes(y = Pente_Trphlvl, x = Pente_Biomass)) +
  geom_point() +
  geom_line(aes(y = .fitted, x = Pente_Biomass), color = "blue") +
  geom_ribbon(aes(
    ymin  = .fitted - .se.fit,
    ymax  = .fitted + .se.fit
  ),
  fill = "blue", alpha = .1
  ) + xlab("Pente biomasse") + ylab("Pente niveau trophique moyen") + 
  geom_vline(xintercept = 0, color="grey") + 
  geom_hline(yintercept = 0, color="grey")

summary(mylm_ter)
#R carré= 0,74. Intercept = -0,0009. Pente = 0,61.
##On voit encore mieux la relation significative (!!!), aucune dt (ntm) <0 quand dt (biomasse) >0 et inversement quasiment aucune non plus.
