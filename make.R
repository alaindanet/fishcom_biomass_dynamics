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
drake::make(plan, keep_going = TRUE)
drake::make(plan)
drake::vis_drake_graph(plan)

#make(plan, parallelism = "future", jobs = 3)

# If you do not change any code or data,
# subsequent make()'s do not build targets.

# Load your targets back into your session with loadd() and readd().
plan
print(plan, n = 40)

loadd(monotonous_data)
loadd(monotonous_data)
loadd(full_data2)
debugonce(get_lm_station)
test <- get_lm_station(.data = monotonous_data, 
    var_name = c(get_biomass_var(), get_com_str_var(all = TRUE)),
    rhs = " ~ nb_year")

install.packages("betapart")
library(betapart)


  #com_test <- left_join(com, select(.op, opcod, surface), by = "opcod")

  mat <- compute_com_mat(.op = .op, com = com, variable = variable,
    summarise_by_station = summarise_by_station)

  cbind(mat[, 1:2], compute_pielou_simpson(.data = mat[, 2:ncol(mat)])) %>%
    as_tibble

}
