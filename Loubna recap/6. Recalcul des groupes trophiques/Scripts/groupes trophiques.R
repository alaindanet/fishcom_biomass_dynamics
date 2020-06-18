library(dplyr)
library(broom)
library(tidyverse)

#Sélectionner la liste metaweb
matrice <- metaweb_analysis$metaweb
#Retirer les 7 lignes ressources
matrix_wo_resources<-matrice[!rownames(matrice)%in%c("det", "biof", "phytopl", "phytob", "macroph", "zoopl", "zoob"),]
#Ne sélectionner que les colSums > 0
colSums(matrix_wo_resources)
matrice.df<-as.data.frame(matrix_wo_resources)

