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

loadd(data)
loadd(net_dyn_lm)
net_dyn_lm %>%
  select(-model) %>%
  filter_at(vars(x, y), any_vars((. != x)))


loadd(net_dyn_lm_coeff)
loadd(temporal_dynamics_coef)

loadd(net_dyn_lm_plot)

cowplot::plot_grid(plotlist = net_dyn_lm_plot$plot, ncol = 3)

test_group <- median_biomass %>%
  filter(coeff == "biomass") %>%
  mutate(com_size = ifelse(bm_med < median(bm_med, na.rm = TRUE), "little", "big")) %>%
  ungroup() %>%
  select(station, com_size)
test <- temporal_dynamics_coef %>%
  left_join(test_group, by = "station") %>%
  group_by(com_size)
extract_xy_slope_from_coeff("biomass", "log_bm", .data = test)

loadd()
debug(compute_lm_temporal_trends)
#TODO: get group
try1 <- compute_lm_temporal_trends(.data = ungroup(test),
    x = c("biomass", "log_bm", "connectance", "w_trph_lvl_avg", "richness", "weighted_connectance"), 
    y = c("weighted_connectance", "connectance", "w_trph_lvl_avg", "richness"), group = com_size)

st_122 <- filter(data, station == 122)
test_trajectory <- data %>%
  select(station, biomass, richness, nb_year, connectance) %>%
  group_by(station) %>%
  nest() 
test <- class.trajectory(Y = st_122$biomass, X = st_122$nb_year)

biomass <- purrr::map_dfr(test_trajectory$data, ~try(class.trajectory(Y = na.omit(.x)$biomass, X = na.omit(.x)$nb_year))) %>%
  as_tibble()

richness <- purrr::map(test_trajectory$data, ~try(class.trajectory(Y = na.omit(.x)$richness, X = na.omit(.x)$nb_year)))
map_chr(richness, class)
test_trajectory$data[[72]]
test_trajectory$data[[126]]

connectance <- purrr::map_dfr(test_trajectory$data, ~try(class.trajectory(Y = na.omit(.x)$connectance, X = na.omit(.x)$connectance))) %>%
  as_tibble()
