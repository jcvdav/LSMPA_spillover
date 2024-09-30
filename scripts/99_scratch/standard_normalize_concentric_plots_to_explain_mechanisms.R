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
  fixest,
  here,
  tidyverse
)

# Source custom functions ------------------------------------------------------
source(here("scripts/00_set_up.R"))

# Load data --------------------------------------------------------------------
annual_panel <- readRDS(file = here("data", "processed", "annual_full_estimation_panel.rds"))
most_relevant_panel <- readRDS(file = here("data", "processed", "annual_relevant_mpa_gears_estimation_panel.rds"))


most_relevant_panel %>%
  filter(gear == "purse_seine") %>%
  drop_na(treatment_100) %>%
  mutate(wdpaid = name) %>%
  group_by(wdpaid, event, treatment_100) %>%
  summarize(cpue_tot = mean(cpue_tot, na.rm = T),
            .groups = "drop") %>%
  group_by(wdpaid, treatment_100) %>%
  mutate(mean_cpue_bef = mean(cpue_tot[event <= 0]),
         sd_cpue_bef = sd(cpue_tot[event <= 0])) %>%
  ungroup() %>%
  mutate(cpue_norm = (cpue_tot - mean_cpue_bef) / sd_cpue_bef,
         treatment_100 = fct_relevel(treatment_100, "near", "far")) %>%
  ggplot(aes(x = event,
             y = wdpaid,
             fill = cpue_norm,
             size = treatment_100)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(shape = 21, color = "white") +
  geom_point(size = 2, color = "white") +
  scale_size_manual(values = c(10, 18)) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white")


most_relevant_panel %>%
  drop_na(treatment_100) %>%
  filter(gear == "purse_seine") %>%
  mutate(wdpaid = name) %>%
  group_by(wdpaid, treatment_100) %>%
  mutate(mean_cpue_bef = mean(cpue_tot[post == 0], na.rm = T),
         sd_cpue_bef = sd(cpue_tot[post == 0], na.rm = T)) %>%
  ungroup() %>%
  group_by(wdpaid, post, treatment_100, mean_cpue_bef, sd_cpue_bef) %>%
  summarize(cpue_tot = mean(cpue_tot, na.rm = T),
            .groups = "drop") %>%
  mutate(cpue_norm = (cpue_tot - mean_cpue_bef) / sd_cpue_bef,
         treatment_100 = fct_relevel(treatment_100, "near", "far")) %>%
  ggplot(aes(x = post == 1,
             y = wdpaid,
             fill = cpue_norm,
             size = treatment_100)) +
  # geom_vline(xintercept = 0.5, linetype = "dashed") +
  geom_point(shape = 21, color = "black") +
  scale_size_manual(values = c(10, 20)) +
  facet_wrap(~wdpaid, scales = "free") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white")

