# This minimal drake workflow demonstrates the file structure from
# https://books.ropensci.org/drake/projects.html#code-files.
# It is not the only way to organize R scripts for drake,
# but this pattern is helpful.

source("R/packages.R")  # Load your packages, e.g. library(drake).
source("R/misc.R") # Define your custom code as a bunch of functions.
source("R/variable_shortcut.R") # Define your custom code as a bunch of functions.
source_dir("R") # Define your custom code as a bunch of functions.
source("R/plan.R")      # Create your drake plan.
# Call make() to run your work.
# Your targets will be stored in a hidden .drake/ cache,
#drake::r_make(plan)
#drake::make(plan, targets = "fig_std_coef",  keep_going = TRUE)
drake::make(plan, keep_going = TRUE)
drake::make(plan)
drake::vis_drake_graph(plan)

make(plan, parallelism = "future", jobs = 3)
rm(list = ls())
gc()

#drake::drake_cache("/home/alain/Documents/post-these/mnhn/fishcom_biomass_dynamics/.drake")$unlock()
# If you do not change any code or data,
# subsequent make()'s do not build targets.
# Load your targets back into your session with loadd() and readd().
plan
print(plan, n = 40)

library(future)
plan(multisession, workers = 3)

loadd(data_tps_sem)
loadd(sem_tps)

semeff <- semEff(sem_tps, R = 10000, seed = 13, parallel = "no")
save(semeff, file = get_mypath("data", "semeff.rda"))

loadd(sp_sem, data_sp_sem)
sp_semeff <- semEff(sp_sem, R = 10000, seed = 13, parallel = "no", type = "parametric")
save(sp_semeff, file = get_mypath("data", "sp_semeff.rda"))

drake::drake_cache("/home/alain/Documents/post-these/mnhn/fishcom_biomass_dynamics/.drake")$unlock()
