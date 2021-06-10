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

#drake::drake_cache("/home/alain/Documents/post-these/mnhn/fishcom_biomass_dynamics/.drake")$unlock()
# If you do not change any code or data,
# subsequent make()'s do not build targets.
# Load your targets back into your session with loadd() and readd().
plan
print(plan, n = 40)

#TODO check rigor of automatic plotting; all the conditions are respected?

library(future)
plan(multisession, workers = 3)

attr(body(get_pred_plot_from_new_model), "srcfile")

loadd(bm_rich_trends, st_sp, sp_st_data, summary_var_med, data_for_pca,
  st_analysis, sp_slope_x_bound, sp_model_bm_rich_mono_trends)
loadd(sp_relation_plot)


debugonce(get_predict_from_new_model)
get_pred_plot_from_new_model(
  model = sp_model_bm_rich_mono_trends,
  dataset = sp_st_data,
  x_bound = sp_slope_x_bound,
  std_error_bar = FALSE
)

loadd(slope_com_var)
loadd(model_summary, comb)
loadd(st_mono_trends_rich_bm, st_trends_rich_bm,  slope_com_var_no_covar)
loadd(model,
  model_bm_rich_no_covar_no_inc,
  model_bm_rich_trends,
  model_bm_rich_mono_trends,
  model_bm_rich,
  resume_bm_rich_mod,
  resume_bm_rich_mod_mono_trends,
  model_summary,
  anova_bm_rich_mod_mono_trends,
  anova_table_bm_rich_mono_trends,
  anova_bm_rich_mod,
  anova_bm_rich_mod_trends,
  anova_bm_rich_mod_mono_stable_trends,
  slope_x_bound,
  predict_table
)

loadd(pred_bm_rich_mono_stable_trends)
loadd(sp_models, slope_x_bound, sp_st_data, st_trends_rich_bm, sp_model_bm_rich_mono_trends)
map(sp_models, get_sp_model_plot)

loadd(sp_slope_x_bound, slope_x_bound)

HERE
debugonce(get_predict_from_new_model)
test <- get_pred_plot_from_new_model(
  model =sp_model_bm_rich_mono_trends ,
  dataset = filter(sp_st_data, station %in% st_trends_rich_bm),
  x_bound = sp_slope_x_bound,
  std_error_bar = FALSE
)
loadd(sp_relation_plot_mono_trends)
sp_relation_plot_mono_trends
test$pred_plot[[1]]

# Test variability of the results according to the number of sites
loadd(sp_models)
test2 <- get_sp_model_plot(model_bm_rich_no_covar_no_inc)
plot_grid(plotlist = test2$gg)
mono_plot <- get_sp_model_plot(model_bm_rich_mono_trends)
plot_grid(plotlist = mono_plot$gg)
mono_plot <- get_sp_model_plot(model_bm_rich_mono_trends)
plot_grid(plotlist = mono_plot$gg)
trends_plot <- get_sp_model_plot(model_bm_rich_trends)
plot_grid(plotlist = trends_plot$gg)
all_plot <- get_sp_model_plot(model_bm_rich)
plot_grid(plotlist = all_plot$gg)

theme_set(theme_cowplot())
p_raw_plot <- map2(
  filter(model, x %in% c("log_bm_std", "log_rich_std"), y %in% names(model_bm_rich_no_covar_no_inc))$x,
  filter(model, x %in% c("log_bm_std", "log_rich_std"), y %in% names(model_bm_rich_no_covar_no_inc))$y,
  ~plot_raw_data_new_model(
    .df = filter(slope_com_var_no_covar, station %in% st_trends_rich_bm),
    x_var = .x, y_var = .y,
    std_error = TRUE, covar = NULL
  )
)
plot_grid(plotlist = p_raw_plot)
plot_raw_data_new_model(
  .df = filter(slope_com_var_no_covar, station %in% st_mono_trends_rich_bm),
  x_var = "log_bm_std", y_var = "log_rich_std",
  std_error = TRUE, covar = NULL
)
