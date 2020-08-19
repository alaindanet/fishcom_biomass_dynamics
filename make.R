# This minimal drake workflow demonstrates the file structure from
# https://books.ropensci.org/drake/projects.html#code-files.
# It is not the only way to organize R scripts for drake,
# but this pattern is helpful.

source("R/packages.R")  # Load your packages, e.g. library(drake).
source("R/misc.R") # Define your custom code as a bunch of functions.
source_dir("R") # Define your custom code as a bunch of functions.
source("R/plan.R")      # Create your drake plan.

# Call make() to run your work.
# Your targets will be stored in a hidden .drake/ cache,
make(plan)

# If you do not change any code or data,
# subsequent make()'s do not build targets.

# Load your targets back into your session with loadd() and readd().
loadd(temporal_dynamics_coef)
temporal_dynamics_coef$value[[1]]
debug(compute_lm_temporal_trends)
library(tibble)
test <- compute_lm_temporal_trends(.data = temporal_dynamics_coef,
    y = c("biomass", "rel_bm"), x = c("connectance", "w_trph_lvl_avg", "richness"))
test <- extract_xy_slope_from_coeff(x = "biomass", y = "rel_bm", .data = temporal_dynamics_coef)

loadd(correlation_temporal_dynamics)
correlation_temporal_dynamics$x
correlation_temporal_dynamics$y
get_lm_coeff(.data = correlation_temporal_dynamics, col_names = "model")
temporal_dynamics
