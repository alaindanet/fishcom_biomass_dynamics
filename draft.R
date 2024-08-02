library(targets)
library(tidyverse)
library(magrittr)
library(cowplot)
library(viridis)
library(arrow)
library(glmmTMB)
library(piecewiseSEM)
library(semEff)
library(lme4)

tar_load(c(sim, sim_std))
tar_load(p_sem_list_sim)
tar_load(semeff_tot)
tar_load(semeff_bioener_bm_rich_tab)
tar_load(sim_cor_plot)
corrplot::corrplot(sim_cor_plot)

tar_load(gaussian_pred_station_tps10)
summary_pred_station_tps10 <-
  gaussian_pred_station_tps10 %>%
  filter(
    response %in% c("log_bm_std", "ct_ff", "w_trph_lvl_avg", "log_rich_std")
    ) %>%
  group_by(response) %>%
  summarise(summ = list(enframe(summary_distribution(mean, na.rm = TRUE)))) %>%
  unnest(summ) %>%
  pivot_wider(names_from = "name", values_from = "value")

summarise(quantile05 = quantile())







c(NULL, list(a = 1))
bm_mod <- lm(total_bm ~ S, sim_std)
tlvl_mod <- lm(weighted_average_trophic_level ~ K + S, sim_std)
ct_mod <- lm(connectance_final ~ K + S, sim_std)


car::vif(bioener_ks_mod_list[[2]])
print(x %>% filter(effect_type == "total"), n = 22)

bioener_bm_rich_mod_list <- list(
  bm = lm(log_total_bm ~ log_final_richness, sim),
  tlvl = lm(log_weighted_average_trophic_level ~ log_total_bm + log_final_richness, sim),
  ct = lm(log_connectance_final ~ log_total_bm + log_final_richness, sim)
)

bioener_bm_rich_fwid_mod_list <- list(
  bm = lmer(log_total_bm ~ log_final_richness + C + S + K + (1|fw_id), sim),
  tlvl = lmer(log_weighted_average_trophic_level ~ log_total_bm + log_final_richness + C + S + K + (1|fw_id), sim),
  ct = lmer(log_connectance_final ~ log_total_bm + log_final_richness + C + S + K + (1|fw_id), sim)
)

sem_bioener_bm_rich_fwid <- as.psem(bioener_bm_rich_fwid_mod_list)
semeff_bioener_bm_rich_fwid <- semEff(sem_bioener_bm_rich_fwid, R = 20, ci.type = "perc", ran.eff = "fw_id")
semeff_bioener_bm_rich_fwid_table <- from_semEff_to_table(x = semeff_bioener_bm_rich_fwid)
print(semeff_bioener_bm_rich_fwid_table %>% filter(effect_type == "total"), n = 22)

bm_mod <- lm(total_bm ~ final_richness, sim_std)
tlvl_mod <- lm(weighted_average_trophic_level ~ total_bm + final_richness, sim_std)
ct_mod <- lm(connectance_final ~ total_bm + final_richness, sim_std)

lm(consumer_biomass ~ consumer_richness, sim_std)
lm(weighted_average_trophic_level ~ consumer_biomass + consumer_richness, sim_std)
lm(connectance_final ~ consumer_richness + consumer_biomass, sim_std)

glmmTMB(total_bm ~ final_richness + (1|fw_id), sim_std)
glmmTMB(weighted_average_trophic_level ~ total_bm + final_richness + (1|fw_id), sim_std)
glmmTMB(connectance_final ~ total_bm + final_richness + (1|fw_id), sim_std)

glmmTMB(total_bm ~ final_richness + (1|fw_id), simhc)
glmmTMB(weighted_average_trophic_level ~ total_bm + final_richness +(1|fw_id), simhc)
glmmTMB(connectance_final ~ total_bm + final_richness + (1|fw_id), simhc)

bm_mod <- lm(total_bm ~ final_richness, sim_std)
tlvl_mod <- lm(weighted_average_trophic_level ~ total_bm + final_richness, sim_std)
ct_mod <- lm(connectance_final ~ total_bm + final_richness, sim_std)

glmmTMB(total_bm ~ S + (1|fw_id), simhc)
glmmTMB(weighted_average_trophic_level ~ K + S + (1|fw_id), simhc)
glmmTMB(connectance_final ~ K + S + (1|fw_id), simhc)

glmmTMB(consumer_biomass ~ consumer_richness + (1|fw_id), sim_std)
glmmTMB(weighted_average_trophic_level ~ consumer_biomass + consumer_richness + (1|fw_id), sim_std)
glmmTMB(connectance_final ~ consumer_richness + consumer_biomass + (1|fw_id), sim_std)

glmmTMB(consumer_biomass ~ consumer_richness + (1|fw_id), simhc)
glmmTMB(weighted_average_trophic_level ~ consumer_biomass + consumer_richness + (1|fw_id), simhc)
glmmTMB(connectance_final ~ consumer_richness + consumer_biomass + (1|fw_id), simhc)

glmmTMB(maximum_trophic_level ~ consumer_biomass + consumer_richness + (1|fw_id), simhc)
glmmTMB(average_trophic_level ~ consumer_biomass + consumer_richness + (1|fw_id), simhc)

lm(total_bm ~ final_richness, simhc)
lm(weighted_average_trophic_level ~ final_richness + total_bm, simhc)
lm(connectance_final ~ final_richness + total_bm, simhc)
lm(connectance_final ~ final_richness + total_bm, simhc)
lm(top_consumer_richness ~ final_richness + total_bm, sim)
lm(maximum_trophic_level ~ final_richness + total_bm, simhc)
lm(average_trophic_level ~ final_richness + total_bm, simhc)

source("R/misc.R") # Define your custom code as a bunch of functions.
source("R/variable_shortcut.R") # Define your custom code as a bunch of functions.
source_dir(get_mypath("R")) # Define your custom code as a bunch of functions.


tar_load(gaussian_pred_station_tps10)
tps10 <- gaussian_pred_station_tps10 %>%
  filter(response %in% c("log_bm_std", "ct_ff", "w_trph_lvl_avg", "log_rich_std")) %>%
  mutate(
    response = var_replacement()[response],
    response = ifelse(
      response %in% c("Species richness", "Biomass"),
      paste0(response, " (%)"), response
      ),
    response_f = factor(response, levels = c("Biomass (%)", "Species richness (%)", "Connectance", "Avg trophic level"))
  )

sum_tps_10 <- tps10 %>%
  group_by(response_f) %>%
  summarise(avg = mean(mean), med = median(mean)) %>%
  pivot_longer(-response_f, names_to = "metric", values_to = "values")

p_tps10 <- tps10 %>%
  ggplot(aes(x = mean)) +
  geom_histogram(fill = "lightblue", bins = 60) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "red") +
  geom_vline(data = filter(sum_tps_10, metric == "med"), aes(xintercept = values)) + #, color = metric
  labs(x = "Temporal trends by decade", y = "Number of sites") +
    scale_x_continuous(n.breaks = 10) +
    facet_wrap(vars(response_f), scales = "free_x",
      ncol = 2, strip.position = "top") +
    theme_half_open() +
    theme(
      strip.background = element_blank(),
      axis.text.x = element_text(size = 8)
    )
ggsave(
       filename = here::here("figures", "tps10.pdf"),
       plot = p_tps10,
       scale = 2.0,
       width = 88,
       height = 88 * .6,
       units = "mm"
)

hyp_tps <- tibble(myx = seq(-100, 100, 10), myy = myx) %>%
  expand.grid() %>%
  ggplot(aes(x = myx, y = myy)) +
  geom_abline(slope = 1, intercept = 0, color = "blue") +
  geom_abline(slope = -1, intercept = 0, color = "red") +
  geom_abline(slope = 0, intercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "gray", linetype = "dashed") +
  xlim(-100, 100) +
  ylim(-100, 100) +
  xlab("Temporal trends in biomass/richness") +
  ylab("Temporal trends in trophic structure") +
  theme_half_open() +
  theme(
    strip.background = element_blank(),
    axis.text = element_text(size = 8)
  )

ggsave(
       filename = here::here("figures", "hyp_tps.pdf"),
       plot = hyp_tps,
       scale = 2.0,
       width = 88,
       height = 88 * .6,
       units = "mm"
)

tar_load(p_list_sem_tps)

p_list_sem_tps$p_bm_rich +
  theme_half_open()
ggsave(
       filename = here::here("figures", "tps_bm_rich.pdf"),
       plot = p_list_sem_tps$p_bm_rich +
         theme_half_open(),
       scale = 2.0,
       width = 88,
       height = 88 * 1,
       units = "mm"
)

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


v_colors <- viridisLite::viridis(n = 2, begin = 0.5, end = 1)
names(v_colors) <- c("tps", "sp")

tps_dec_inc <- map(c("log_bm_std", "log_rich_std", "ct_ff", "w_trph_lvl_avg"), #
  function(x) {
    tmp <- plot_temporal_variable_lm(
      st = "9561",
      var = x,
      add_median = FALSE,
      col_median = v_colors["sp"],
      col_trend = v_colors["tps"]
      ) +
    theme(axis.title = element_text(size = 12))
  tmp$layers[[1]]$aes_params$size <- 4
  tmp
  }
)

ex_tps_net <- plot_grid(plotlist = tps_dec_inc)
ggsave(
  filename = here::here("figures", "example_tps_net.pdf"),
  plot = ex_tps_net,
  scale = 2.0,
  width = 88,
  height = 88 * .8,
  units = "mm"
)

tar_load(semeff_tot)

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
