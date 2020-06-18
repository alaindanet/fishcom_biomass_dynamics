#Scatter plot residuals vs fitted 
plot(mylm, 1) #richness
plot(mylm_bis, 1) #connectance
plot(mylm_ter, 1) #n.t.m

#Tests de Breusch Pagan
ncvTest(mylm)
ncvTest(mylm_bis)
ncvTest(mylm_ter)

#Normalité des résidus
hist(resid(mylm))
hist(resid(mylm_bis))
hist(resid(mylm_ter))

