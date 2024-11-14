library(targets)
library(tidyverse)
library(magrittr)
library(lubridate)
library(cowplot)
library(viridis)
library(arrow)
library(glmmTMB)
library(piecewiseSEM)
library(semEff)
library(lme4)
library(INLA)
lapply(list.files(here::here("R"), full.names = TRUE), source)

map2(p_list_sem_tps, names(p_list_sem_tps),
  function(p, name) {
    tmp <- p +
      theme_half_open()
    p_name <- str_replace(name, "p_", "tps_")

    ggsave(
      filename = here::here("figures", paste0(p_name, ".pdf")),
      plot = tmp,
      scale = 2.0,
      width = 88,
      height = 88 * .8,
      units = "mm"
    )
  }
)

tar_load(full_data2)

v_colors <- viridisLite::viridis(n = 2, begin = 0.5, end = 1)
names(v_colors) <- c("tps", "sp")

tps_dec_inc <- map(c("log_bm_std", "log_rich_std", "prop_redundant_links", "connectance", "w_trph_lvl_avg"), #
  function(x) {
    tmp <- plot_temporal_variable_lm(
      st = "9561",
      var = x,
      add_median = FALSE,
      col_median = v_colors["sp"],
      col_trend = v_colors["tps"]
      ) +
    theme_half_open() +
    theme(axis.title = element_text(size = 12))
  tmp$layers[[1]]$aes_params$size <- 4
  tmp
  }
)

ex_tps_net <- plot_grid(plotlist = tps_dec_inc)
ggsave_multiple(
  paste0("example_tps_net", c(".png", ".pdf")),
  plot = ex_tps_net,
  path = here::here("figures"),
  scale = 1.8,
  width = 120,
  height = 120 * .6,
  units = "mm")

tar_load(semeff_tot)

# Talk EEB sheffield
tps_dec_inc <- map(c("log_bm_std", "log_rich_std", "prop_redundant_links"), #
  function(x) {
    tmp <- plot_temporal_variable_lm(
      st = "9561",
      var = x,
      add_median = FALSE,
      col_median = v_colors["sp"],
      col_trend = v_colors["tps"]
      ) +
    theme_half_open() +
    theme(axis.title = element_text(size = 12))
  tmp$layers[[1]]$aes_params$size <- 4
  tmp
  }
)
ex_tps_net <- plot_grid(plotlist = tps_dec_inc, nrow = 2)
ggsave_multiple(
  paste0("example_tps_net_redun", c(".png", ".pdf")),
  plot = ex_tps_net,
  path = here::here("figures"),
  scale = 1.8,
  width = 80,
  height = 80 * 1,
  units = "mm")

tar_load(p_list_sem_tps)

sem_tps_redun <- map(p_list_sem_tps[c("p_redundant_bm", "p_redundant_rich")],
  ~.x + theme_bw()
)

ggsave_multiple(
  paste0("example_tps_net_redun", c(".png", ".pdf")),
  plot = plot_grid(plotlist = sem_tps_redun),
  path = here::here("figures"),
  scale = 1.8,
  width = 80,
  height = 80 * .45,
  units = "mm")

p_semeff_tot <- semeff_tot %>%
  filter(effect_type == "total", !predictor %in% c("log_RC1", "log_RC2")) %>%
  mutate(
    response = str_remove(response, "mean_"),
    response = var_replacement()[response],
    predictor = str_remove(predictor, "mean_"),
    predictor = var_replacement()[predictor],
    effect_type = str_to_sentence(effect_type),
    model = str_to_sentence(model)
    ) %>%
  ggplot(aes(y = predictor, x = effect, color = response, shape = model)) +
  geom_point(position = position_dodge(width = 0.4), size = 4) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_linerange(
    aes(xmin = lower_ci, xmax = upper_ci),
    size = 1,
    position = position_dodge(width = 0.4)
    ) +
  labs(
    y = "Predictor",
    x = expression(paste("Standardized slope coefficients ", r[delta])),
    color = "Foodweb metric", shape = "Model") +
  theme_cowplot() +
  #background_grid() +
  theme(
    legend.position = c(.05, .05),
    legend.justification = c("left", "bottom"),
    strip.background = element_blank(),
    axis.title.y = element_blank()
  ) +
guides(color = guide_legend(override.aes = list(shape = NA)))

ggsave(
  filename = here::here("figures", "sem_total_effect.pdf"),
  plot = p_semeff_tot,
  scale = 1.5,
  width = 88,
  height = 88 * .8,
  units = "mm"
)
