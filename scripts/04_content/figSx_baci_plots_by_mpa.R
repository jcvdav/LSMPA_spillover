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

# Load data --------------------------------------------------------------------

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------



# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  cowplot,
  ggimage,
  tidyverse
)

# Source custom funcions -------------------------------------------------------
source(here("scripts/00_set_up.R"))

img <- function(pic) {here("data", "raw", "gear_fish_pics", paste0(pic, ".svg"))}

# Load data --------------------------------------------------------------------
most_relevant_panel <- readRDS(file = here("data", "processed", "annual_relevant_mpa_gears_estimation_panel.rds"))

## PROCESSING ##################################################################
ps_data <- most_relevant_panel %>%
  filter(gear == "purse_seine") %>%
  rename(cpue = cpue_tot)

ll_data <- most_relevant_panel %>%
  filter(gear == "longline") %>%
  rename(cpue = cpue_tot)

# Build Delta cpue dfs ---------------------------------------------------------
ps_delta_cpue <- ps_data %>%
  group_by(name) %>%
  nest() %>%
  mutate(delta = map(data, delta_cpue)) %>%
  select(name, delta) %>%
  unnest(delta) %>%
  mutate(treatment = ifelse(near == 1, "Near", "Far"))

ll_delta_cpue <- ll_data %>%
  group_by(name) %>%
  nest() %>%
  mutate(delta = map(data, delta_cpue)) %>%
  select(name, delta) %>%
  unnest(delta) %>%
  mutate(treatment = ifelse(near == 1, "Near", "Far"))

## VISUALIZE ###################################################################

# BACI plots -------------------------------------------------------------------
ps_baci_plot <- baci_plot(data = ps_data) +
  facet_wrap(~name, scale = "free_y") +
  geom_text(data = ps_delta_cpue,
            aes(x = 2,
                y = post_1 * (0.5 + near),
                label = paste0(round(change, 2), "%"),
                color = treatment),
            inherit.aes = F)

ll_baci_plot <- baci_plot(data = ll_data) +
  facet_wrap(~name, scale = "free_y") +
  geom_text(data = ll_delta_cpue,
            aes(x = 2,
                y = post_1 * (0.5 + near),
                label = paste0(round(change, 2), "%"),
                color = treatment),
            inherit.aes = F)

startR::lazy_ggsave(
  plot = ps_baci_plot,
  filename = "figSx_ps_baci_by_mpa",
  width = 18,
  height = 9
)

startR::lazy_ggsave(
  plot = ll_baci_plot,
  filename = "figSx_ll_baci_by_mpa",
  width = 18,
  height = 13.5
)
