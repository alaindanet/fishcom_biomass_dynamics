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

# If you do not change any code or data,
# subsequent make()'s do not build targets.
# Load your targets back into your session with loadd() and readd().
plan
print(plan, n = 40)

#TODO check rigor of automatic plotting; all the conditions are respected?

library(future)
plan(multisession, workers = 3)

load("data/community_metrics.rda")
loadd(st_mono_trends_combined_list)
print(st_mono_trends_combined_list, n = 20)

loadd(data4model)
test <- data4model %>%
  filter(x == "log_rich_std", y %in% c("piel_bm", "piel_nind","w_trph_lvl_avg", "log_bm_std")) %>%
  unnest(data_model) %>%
  select(station ,y, log_rich_std_slope, linear_slope) %>%
  pivot_wider(names_from = "y", values_from = "linear_slope")

mod <- lm(w_trph_lvl_avg ~ log_rich_std_slope + log_bm_std, data = test)
summary(mod)
car::vif(mod)

# Is it the same for pielou ?

mod <- lm(piel_bm ~ log_rich_std_slope + log_bm_std, data = test2)
summary(mod)
car::vif(mod)

mod <- lm(piel_nind ~ log_rich_std_slope + log_bm_std, data = test2)
summary(mod)
car::vif(mod)
