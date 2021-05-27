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

#drake::drake_cache("/home/alain/Documents/post-these/mnhn/fishcom_biomass_dynamics/.drake")$unlock()

make(plan, parallelism = "future", jobs = 3)

# If you do not change any code or data,
# subsequent make()'s do not build targets.
# Load your targets back into your session with loadd() and readd().
plan
print(plan, n = 40)

#TODO check rigor of automatic plotting; all the conditions are respected?

library(future)
plan(multisession, workers = 3)

attr(body(get_rich_vs_network_trends), "srcfile")

loadd(sp_models)
map(sp_models, ~map(.x, ~broom::glance(.x)))
loadd(comb, rigal_classification)

get_st_mono_trends(.df = rigal_classification, xvar = "log_bm_std")
get_st_mono_trends(.df = rigal_classification, xvar = "bm_std")
get_st_mono_trends(.df = rigal_classification, xvar = "rich_std")
get_st_mono_trends(.df = rigal_classification, xvar = "log_rich_std")

comb %>%
    mutate(
      data_model = map2(x, y, function (myx, myy, classif) {
	get_y_versus_x_trends(classif = classif, x_var = myx, y_var = myy) %>%
	  filter(station %in% get_st_mono_trends(.df = classif, xvar = myx)$station)
      }, classif = rigal_classification),
      data_model = map2(data_model, covar,
	~left_join(.x, summary_var_f3y[, colnames(summary_var_f3y) %in% c("station", .y)], by = "station")
      )
    )
loadd(predict_plot2)
get_plot_fig1_2(predict_plot = predict_plot2)

