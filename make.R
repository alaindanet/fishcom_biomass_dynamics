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
loadd(model_summary)
loadd(summary_table)


summary_table %>% filter(x == "log_bm_std") %>%
 select(-x, -covar, -model, -model_obj) %>%

 list(reg_table = map_dfr(.$reg_table, rbind),
   anova_table = map_dfr(.$anova_table, rbind)
 )

get_table_from_summary(
    .data = model_summary,
    model =  "mod_medium_bm",
    variable = NULL
  )

# If you do not change any code or data,
# subsequent make()'s do not build targets.

# Load your targets back into your session with loadd() and readd().
plan
print(plan, n = 40)

loadd(model)
loadd(predict_plot, summary_table, predict_table, slope_x_bound)

