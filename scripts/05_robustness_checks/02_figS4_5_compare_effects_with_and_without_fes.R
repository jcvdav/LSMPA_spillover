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
mpa_regs <- readRDS(file = here("data", "output", "gear_mpa_regs.rds"))

# And now spp models
spp_regs <- readRDS(file = here("data", "output", "gear_spp_regs.rds"))

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
mpa_regs_wo_fe <- feols(log(cpue_tot) ~ post + near + post:near,
                        panel.id = ~id + year,
                        vcov = conley(cutoff = 200),
                        split = ~paste(nice_gear, short_name),
                        subset = ~gear == "purse_seine",
                        data = most_relevant_panel)

# Species-level analysis -------------------------------------------------------
spp_regs_wo_fe <- feols(log(cpue_tot) ~ post + near + post:near,# | chagos,
                        panel.id = ~id + year,
                        vcov = conley(cutoff = 200),
                        data = panel_for_spp_regs %>%
                          mutate(chagos = ifelse(wdpaid == "555512151" & nice_gear == "PS", 1, 0)),
                        subset = ~nice_gear == "PS",
                        split = ~paste(nice_gear, spp))

## EXTRACT models ##############################################################
# Extract coefficients into a data.frame ---------------------------------------
# For MPA analysis
relevant_by_mpa_df <- extract_mpa_coefs(mpa_regs)                          # With fixed effects
relevant_by_mpa_df_wo_fe <- extract_mpa_coefs(mpa_regs_wo_fe)              # Without fixed effects

# For Species analysis
spp_df <- extract_spp_coefs(spp_regs)                                 # With fixed effects
spp_df_wo_fe <- extract_spp_coefs(spp_regs_wo_fe)                     # Without fixed effects

## VISUALIZE ###################################################################
# For MPA-level analysis ------------------------------------------------------
mpa_compare_models <- bind_rows(relevant_by_mpa_df, relevant_by_mpa_df_wo_fe) %>%
  ggplot(aes(x = mpa, y = estimate, fill = model)) +
  geom_pointrange(aes(ymin = estimate - std.error,
                      ymax = estimate + std.error),
                  position = position_dodge(width = 0.5),
                  shape = 21) +
  coord_flip() +
  scale_shape_manual(values = gear_shapes) +
  scale_fill_manual(values = fe_palette) +
  guides(fill = guide_legend(override.aes = list(shape = 21))) +
  geom_hline(yintercept = 0) +
  theme(legend.position = c(0, 0),
        legend.justification = c(0, 0),
        legend.box = "horizontal",
        axis.title.y = element_blank()) +
  labs(y = "Effect on CPUE",
       shape = "Gear",
       fill = "Model")

# For species-level analysis
spp_compare_models <- bind_rows(spp_df, spp_df_wo_fe) %>%
  ggplot(aes(x = spp, y = estimate, fill = model)) +
  geom_pointrange(aes(ymin = estimate - std.error,
                      ymax = estimate + std.error),
                  position = position_dodge(width = 0.5),
                  shape = 21) +
  coord_flip() +
  scale_fill_manual(values = fe_palette) +
  guides(fill = guide_legend(override.aes = list(shape = 21))) +
  geom_hline(yintercept = 0) +
  theme(legend.position = c(1, 0),
        legend.justification = c(1, 0),
        legend.box = "horizontal",
        axis.title.y = element_blank()) +
  labs(y = "Effect on CPUE",
       shape = "Gear",
       fill = "Model")

# EXPORT ######################################################################
# Supplementary figures --------------------------------------------------------
# Comparing FE and WOFE for MPAs
startR::lazy_ggsave(plot = mpa_compare_models,
                    filename = "figS4_effects_by_mpa_with_and_without_fe",
                    width = 12,
                    height = 6)

# Comparing FE and WOFE for spp
startR::lazy_ggsave(plot = spp_compare_models,
                    filename = "figS5_effects_by_spp_with_and_without_fe",
                    width = 10,
                    height = 6)
