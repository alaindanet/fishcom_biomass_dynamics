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

make(plan, parallelism = "future", jobs = 3)
rm(list = ls())
gc()

loadd(full_data2, summary_var_med, st_mono_trends_stable_rich_bm)

debugonce(get_range_variable_plot)
get_range_variable_plot(
    full_data = full_data2,
    sp_data = summary_var_med,
    st = st_mono_trends_stable_rich_bm,
    var_to_keep = c("ct_ff", "w_trph_lvl_avg", "log_rich_std", "log_bm_std",
      "piel_bm", "piel_nind")
  )

#drake::drake_cache("/home/alain/Documents/post-these/mnhn/fishcom_biomass_dynamics/.drake")$unlock()
# If you do not change any code or data,
# subsequent make()'s do not build targets.
# Load your targets back into your session with loadd() and readd().
plan
print(plan, n = 40)

library(future)
plan(multisession, workers = 3)

attr(body(get_pred_plot_from_new_model), "srcfile")


loadd(rigal_classification, st_mono_trends_stable_rich_bm)

