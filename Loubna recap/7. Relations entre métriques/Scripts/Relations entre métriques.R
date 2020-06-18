#Importation des packages
library(dplyr)
library(tidyverse)
library(broom)
library(cowplot)

##### 1. AVEC LES PENTES SIGNIFICATIVES SEULEMENT

#CONNECTANCE ~ RICHESSE SPÉCIFIQUE -> cor
mylm_cor <- lm(Pente_Connectance ~ Pente_Richness, data = all_dt)
data_to_plot_cor <- augment(mylm_cor) 

ggplot(data_to_plot_cor, aes(y = Pente_Connectance, x = Pente_Richness)) +
  geom_point() +
  geom_line(aes(y = .fitted, x = Pente_Richness), color = "blue") +
  geom_ribbon(aes(
    ymin  = .fitted - .se.fit,
    ymax  = .fitted + .se.fit
  ),
  fill = "blue", alpha = .1
  ) + xlab("Pente richesse spécifique") + ylab("Pente connectance") + 
  geom_vline(xintercept = 0, color="grey") + 
  geom_hline(yintercept = 0, color="grey")

summary(mylm_cor)

#CONNECTANCE ~ NIVEAU TROPHIQUE MOYEN -> cot
mylm_cot <- lm(Pente_Connectance ~ Pente_Trphlvl, data = all_dt)
data_to_plot_cot <- augment(mylm_cot) 

ggplot(data_to_plot_cot, aes(y = Pente_Connectance, x = Pente_Trphlvl)) +
  geom_point() +
  geom_line(aes(y = .fitted, x = Pente_Trphlvl), color = "blue") +
  geom_ribbon(aes(
    ymin  = .fitted - .se.fit,
    ymax  = .fitted + .se.fit
  ),
  fill = "blue", alpha = .1
  ) + xlab("Pente niveau trophique moyen") + ylab("Pente connectance") + 
  geom_vline(xintercept = 0, color="grey") + 
  geom_hline(yintercept = 0, color="grey")

summary(mylm_cot)

#NIVEAU TROPHIQUE MOYEN ~ RICHESSE -> tr
mylm_tr <- lm(Pente_Trphlvl ~ Pente_Richness, data = all_dt)
data_to_plot_tr <- augment(mylm_tr) 

ggplot(data_to_plot_tr, aes(y = Pente_Trphlvl, x = Pente_Richness)) +
  geom_point() +
  geom_line(aes(y = .fitted, x = Pente_Richness), color = "blue") +
  geom_ribbon(aes(
    ymin  = .fitted - .se.fit,
    ymax  = .fitted + .se.fit
  ),
  fill = "blue", alpha = .1
  ) + xlab("Pente richesse") + ylab("Pente niveau trophique moyen") + 
  geom_vline(xintercept = 0, color="grey") + 
  geom_hline(yintercept = 0, color="grey")

summary(mylm_tr)

##### 2. AVEC TOUTES LES PENTES (SANS LE FILTRE ALPHA = 5%)

#CONNECTANCE ~ RICHESSE SPÉCIFIQUE -> cor_bis
mylm_cor_bis <- lm(Pente_Connectance ~ Pente_Richness, data = all_unfiltered_dt)
data_to_plot_cor_bis <- augment(mylm_cor_bis) 

ggplot(data_to_plot_cor_bis, aes(y = Pente_Connectance, x = Pente_Richness)) +
  geom_point() +
  geom_line(aes(y = .fitted, x = Pente_Richness), color = "blue") +
  geom_ribbon(aes(
    ymin  = .fitted - .se.fit,
    ymax  = .fitted + .se.fit
  ),
  fill = "blue", alpha = .1
  ) + xlab("Pente richesse spécifique") + ylab("Pente connectance") + 
  geom_vline(xintercept = 0, color="grey") + 
  geom_hline(yintercept = 0, color="grey")

summary(mylm_cor_bis)

#CONNECTANCE ~ NIVEAU TROPHIQUE MOYEN -> cot_bis
mylm_cot_bis <- lm(Pente_Connectance ~ Pente_Trphlvl, data = all_unfiltered_dt)
data_to_plot_cot_bis <- augment(mylm_cot_bis) 

ggplot(data_to_plot_cot_bis, aes(y = Pente_Connectance, x = Pente_Trphlvl)) +
  geom_point() +
  geom_line(aes(y = .fitted, x = Pente_Trphlvl), color = "blue") +
  geom_ribbon(aes(
    ymin  = .fitted - .se.fit,
    ymax  = .fitted + .se.fit
  ),
  fill = "blue", alpha = .1
  ) + xlab("Pente niveau trophique moyen") + ylab("Pente connectance") + 
  geom_vline(xintercept = 0, color="grey") + 
  geom_hline(yintercept = 0, color="grey")

summary(mylm_cot_bis)

#NIVEAU TROPHIQUE MOYEN ~ RICHESSE -> tr_bis
mylm_tr_bis <- lm(Pente_Trphlvl ~ Pente_Richness, data = all_unfiltered_dt)
data_to_plot_tr_bis <- augment(mylm_tr_bis) 

ggplot(data_to_plot_tr_bis, aes(y = Pente_Trphlvl, x = Pente_Richness)) +
  geom_point() +
  geom_line(aes(y = .fitted, x = Pente_Richness), color = "blue") +
  geom_ribbon(aes(
    ymin  = .fitted - .se.fit,
    ymax  = .fitted + .se.fit
  ),
  fill = "blue", alpha = .1
  ) + xlab("Pente richesse") + ylab("Pente niveau trophique moyen") + 
  geom_vline(xintercept = 0, color="grey") + 
  geom_hline(yintercept = 0, color="grey")

summary(mylm_tr_bis)
