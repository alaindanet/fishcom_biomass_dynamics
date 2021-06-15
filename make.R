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

#drake::drake_cache("/home/alain/Documents/post-these/mnhn/fishcom_biomass_dynamics/.drake")$unlock()
# If you do not change any code or data,
# subsequent make()'s do not build targets.
# Load your targets back into your session with loadd() and readd().
plan
print(plan, n = 40)

loadd(model_bm_rich_mono_stable_trends)
test <- model_bm_rich_mono_stable_trends[[3]]

stdse.lm(test)


loadd(
  sp_reg_table_bm_rich_mono_stable_trends,
  reg_table_bm_rich_mono_stable_trends,
  std_table_bm_rich_mono_stable_trends
  )

std_table_bm_rich_mono_stable_trends %>%
  filter(term %in% c("log_rich_std", "log_bm_std")) %>%
  mutate_at(vars(term, response), ~str_replace_all(., var_replacement())) %>%
  ggplot(aes(y = std_estimate, x = response, fill = type)) +
  facet_grid(cols = vars(term)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(x = "Response", y = "Standardized estimate")

library(future)
plan(multisession, workers = 3)

attr(body(get_pred_plot_from_new_model), "srcfile")

loadd(bm_rich_trends, st_sp, sp_st_data, summary_var_med, data_for_pca,
  st_analysis, sp_slope_x_bound, sp_model_bm_rich_mono_trends)


theme_set(theme_cowplot())

loadd(model_bm_rich_trends, slope_com_var_no_covar, st_trends_rich_bm, slope_x_bound)
plot_raw_data_new_model(
          .df = filter(slope_com_var_no_covar, station %in% st_trends_rich_bm),
          x_var = "log_bm_std", y_var = "ct_ff",
          std_error = TRUE,
          covar = NULL
          )
pred_bm_rich_trends <- get_pred_plot_from_new_model(
  model = model_bm_rich_trends,
  dataset = filter(slope_com_var_no_covar, station %in% st_trends_rich_bm),
  x_bound = slope_x_bound, std_error_bar = TRUE
  )
pred_bm_rich_trends$raw_plot[[3]]

fig_test <- get_plot_rich_bm(
  predict_plot = pred_bm_rich_mono_stable_trends,
  get_list = FALSE,
  rm_legend = FALSE,
  bm_x = "log_bm_std",
  bm_y = c(get_com_str_var(), "log_rich_std", "piel_nind", "piel_bm"),
  rich_y = c(get_com_str_var(), "log_bm_std", "piel_nind", "piel_bm"),
  rich_x = "log_rich_std"
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
loadd(sp_models, slope_x_bound, sp_st_data, st_trends_rich_bm,
  sp_model_bm_rich_mono_trends)
sp_models$log_rich_std
