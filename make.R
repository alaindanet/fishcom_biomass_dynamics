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
vis_drake_graph(plan)

# If you do not change any code or data,
# subsequent make()'s do not build targets.

# Load your targets back into your session with loadd() and readd().
plan

loadd(net_dyn_lm_plot)
cowplot::plot_grid(plotlist = net_dyn_lm_plot$plot)
loadd(temporal_dynamics_plot)
temporal_dynamics_plot %<>%
  arrange(variable, station)
to_plot <- temporal_dynamics_plot %>%
  filter(variable == "rel_bm") %>%
  slice(1:9)
cowplot::plot_grid(plotlist = to_plot$plot, labels = to_plot$station)


loadd(temporal_dynamics)
loadd(data)
data %>%
  filter(station == 190)
test <- temporal_dynamics[temporal_dynamics$station == 190, ]$rel_bm[[1]]

lm(rel_bm ~ nb_year + surface, temporal_dynamics[temporal_dynamics$station == 190, ]$data[[1]])
lm(rel_bm ~ nb_year, temporal_dynamics[temporal_dynamics$station == 190, ]$data[[1]])

get_lm_station(.data = data, 
    var_name = c("biomass", "rel_bm", "connectance", "w_trph_lvl_avg", "richness"),
    rhs = " ~ nb_year + offset(surface)")
