################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Description
#
################################################################################

## SET UP ######################################################################
# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  fixest,
  tidyverse
)

# Source custom functions ------------------------------------------------------
source(here("scripts/00_set_up.R"))

# Load data --------------------------------------------------------------------
most_relevant_panel <- readRDS(file = here("data", "processed", "annual_relevant_mpa_gears_estimation_panel.rds"))

gear_stats <- readRDS(file = here("data", "output", "relevant_mpa_gear_combination_model_coefs.rds"))

# Now load the model objects (they are multifit or lists)
# From MPA analysis
gear_mpa_regs <- readRDS(file = here("data", "output", "gear_mpa_regs.rds"))

# And now spp models
gear_spp_regs <- readRDS(file = here("data", "output", "gear_spp_regs.rds"))

## PROCESSING ##################################################################
# We will need this panel for the species-level regressions
panel_for_spp_regs <- most_relevant_panel %>%
  mutate(cpue_skj = ifelse(gear == "longline", 0, cpue_skj)) %>%
  select(wdpaid, short_name, year, event, id, lat, lon, nice_gear, flag, effort, effort_measure, near, post, cpue_alb, cpue_bet, cpue_skj, cpue_yft, -cpue_tot) %>%
  pivot_longer(cols = contains("cpue"), names_to = "spp", values_to = "cpue_tot") %>%
  filter(cpue_tot > 0) %>%
  drop_na(cpue_tot)

# MPA-level analysis -----------------------------------------------------------
# Model without fixed effects
gear_mpa_regs_wo_fe <- feols(log(cpue_tot) ~ post + near + post:near,
                             panel.id = ~id + year,
                             vcov = function(x)vcov_conley_hac(x, id = ~id,
                                                               time = ~year,
                                                               lat = ~lat,
                                                               lon = ~lon,
                                                               cutoff = 200,
                                                               lag = 5),
                             split = ~paste(nice_gear, short_name),
                             data = most_relevant_panel)

# Species-level analysis -------------------------------------------------------
gear_spp_regs_wo_fe <- feols(log(cpue_tot) ~ post + near + post:near | id + year + effort_measure,
                             panel.id = ~id + event,
                             vcov = function(x)vcov_conley_hac(x, id = ~id,
                                                               time = ~year,
                                                               lat = ~lat,
                                                               lon = ~lon,
                                                               cutoff = 200,
                                                               lag = 5),
                             data = panel_for_spp_regs,
                             split = ~paste(nice_gear, spp))

## EXTRACT models ##############################################################
# Extract coefficients into a data.frame ---------------------------------------
# For MPA analysis
relevant_by_mpa_df <- extract_mpa_coefs(gear_mpa_regs)                          # With fixed effects
relevant_by_mpa_df_wo_fe <- extract_mpa_coefs(gear_mpa_regs_wo_fe)              # Without fixed effects

# For Species analysis
gear_spp_df <- extract_spp_coefs(gear_spp_regs)                                 # With fixed effects
gear_spp_df_wo_fe <- extract_spp_coefs(gear_spp_regs_wo_fe)                     # Without fixed effects

## VISUALIZE ###################################################################
# For MPA-level analysis ------------------------------------------------------
mpa_compare_models <- bind_rows(relevant_by_mpa_df, relevant_by_mpa_df_wo_fe) %>%
  mutate(mpa_gear = paste(mpa, gear)) %>%
  ggplot(aes(x = gear, y = estimate, shape = gear, fill = model)) +
  geom_pointrange(aes(ymin = estimate - std.error,
                      ymax = estimate + std.error),
                  position = position_dodge(width = 0.5)) +
  coord_flip() +
  scale_shape_manual(values = gear_shapes) +
  scale_fill_manual(values = fe_palette) +
  guides(fill = guide_legend(override.aes = list(shape = 21))) +
  geom_hline(yintercept = 0) +
  facet_wrap(~mpa, ncol = 4) +
  theme(legend.position = c(1, 0),
        legend.justification = c(1, 0),
        legend.box = "horizontal") +
  labs(y = "Effect on CPUE",
       shape = "Gear",
       fill = "Model")

# For species-level analysis
spp_compare_models <- bind_rows(gear_spp_df, gear_spp_df_wo_fe) %>%
  ggplot(aes(x = spp, y = estimate, shape = gear, fill = model)) +
  geom_pointrange(aes(ymin = estimate - std.error,
                      ymax = estimate + std.error),
                  position = position_dodge(width = 0.5)) +
  coord_flip() +
  scale_shape_manual(values = gear_shapes) +
  scale_fill_manual(values = fe_palette) +
  guides(fill = guide_legend(override.aes = list(shape = 21))) +
  geom_hline(yintercept = 0) +
  facet_wrap(~gear) +
  labs(y = "Effect on CPUE",
       shape = "Gear",
       fill = "Model")

# EXPORT ######################################################################
# Supplementary figures --------------------------------------------------------
# Comparing FE and WOFE for MPAs
startR::lazy_ggsave(plot = mpa_compare_models,
                    filename = "figS4_effects_by_mpa_with_and_without_fe",
                    width = 15,
                    height = 10)
# Comparing FE and WOFE for spp
startR::lazy_ggsave(plot = spp_compare_models,
                    filename = "figS5_effects_by_spp_with_and_without_fe",
                    width = 12,
                    height = 8)
