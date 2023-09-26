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

## VISUALIZE ###################################################################

# BACI plots -------------------------------------------------------------------
ps_baci_plot <- baci_plot(data = ps_data) +
  facet_wrap(~name, scale = "free_y")

ll_baci_plot <- baci_plot(data = ll_data) +
  facet_wrap(~name, scale = "free_y")

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
