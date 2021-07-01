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

library(future)
plan(multisession, workers = 3)

attr(body(get_pred_plot_from_new_model), "srcfile")

loadd(pred_bm_rich_mono_stable_trends)
ti <- pred_bm_rich_mono_stable_trends[
  pred_bm_rich_mono_stable_trends$response == "w_trph_lvl_avg" &
  pred_bm_rich_mono_stable_trends$term == "log_bm_std",
  ]$pred_plot[[1]]
te <- ti +
  labs(
    y = "Average trophic level\ntemporal trends",
    x = "Log biomass\ntemporal trends"
  )
save_plot("figures/tlvl_bm_loubna.png", te,
  base_height = 4,
  base_width = 7
)

# 3D plots:
#https://www.mzes.uni-mannheim.de/socialsciencedatalab/article/regplane3d/
#plotly package:
#https://stackoverflow.com/a/51415659

#TODO: remove pielou of abundances
#- Try 3D plots
#- New figures:
#  - 1) biomass vs richness
#  - 2) food-web structure vs biomass and richness + suppr pielou abundances
#  - Appendix: Spatial relationships
# - Bonus: couverture spatiale des stations qui diminuent ou qui augmentent en
# biomass et en nombre d'espèces
#
loadd(pred_bm_rich_mono_stable_trends)
tii <- pred_bm_rich_mono_stable_trends %>%
  filter(term == "log_rich_std") %>%
  mutate(pred_dbl = map(model,
      ~ggpredict(.x, terms = c("log_rich_std", "log_bm_std")) %>%
        as_tibble()
        )) %>%
  select(response, pred_dbl) %>%
  unnest(pred_dbl) %>%
  rename(log_rich_std_x = x, log_bm_std_x = group) %>%
  pivot_wider(names_from = "response", values_from = "predicted") %>%
  arrange(ct_ff, log_rich_std_x)

ti <- pred_bm_rich_mono_stable_trends %>%
  mutate(pred_plot_color = pmap(
      list(raw_plot, response, term),
      function(p, re, te) {
        p +
          geom_point(data = p$data,
            aes_string(
              y = re,
              x = te,
              color = ifelse(
                te == "log_bm_std",
                "log_rich_std", "log_bm_std"
              )
            )
          )
      }
      ))

p_col_bm <- plot_grid(plotlist = ti[ti$term == "log_bm_std", ]$pred_plot_color)
p_col_rich <- plot_grid(plotlist = ti[ti$term == "log_rich_std", ]$pred_plot_color)

tii$pred_dbl

mat <- reshape2::acast(pred[[3]], x~group, value.var = "predicted")
library(rgl)

te <- ti$prediction[[4]]
persp(x = as.numeric(colnames(mat)),
  y = as.numeric(rownames(mat)),
  z = mat,
  xlab = "Species richness",
  ylab = "Community biomass",
  zlab = "Predicted",
  ticktype = 'detailed',
  theta = 60,
  phi = 20,
  col = "green", shade = 0.5)
