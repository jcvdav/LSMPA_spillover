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
  cowplot,
  ggiplot,
  tidyverse
)

# Source custom funcions -------------------------------------------------------
source(here("scripts/00_set_up.R"))

# Load data --------------------------------------------------------------------
# Purse seine models -----------------------------------------------------------
ps_mod <- readRDS(file = here("data", "output", "spatial_event_study_ps.rds"))

# Longline models --------------------------------------------------------------
ll_mod <- readRDS(file = here("data", "output", "spatial_event_study_ll.rds"))

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
ps_plot <- ggiplot(ps_mod,
                   # multi_style = "facet",
                   geom_style = "ribbon",
                   col = unname(gear_palette)[1],
                   ci_level = c(.8, .95),
                   theme = ggplot2::theme_get()) +
  geom_vline(xintercept = 0) +
  geom_vline(xintercept = 100, linetype = "dashed") +
  geom_point(shape = 21, size = 4) +
  labs(x = "Distance bin (nautical miles)",
       title = NULL) +
  theme(legend.position = "None")

ll_plot <- ggiplot(ll_mod,
                   # multi_style = "facet",
                   geom_style = "ribbon",
                   col = unname(gear_palette)[2],
                   ci_level = c(.8, .95),
                   theme = ggplot2::theme_get()) +
  geom_vline(xintercept = 0) +
  geom_vline(xintercept = 300, linetype = "dashed") +
  geom_point(shape = 22, size = 4) +
  labs(x = "Distance bin (nautical miles)",
       title = NULL) +
  theme(legend.position = "None")


p <- plot_grid(ps_plot,
               ll_plot,
               ncol = 1,
               labels = "AUTO") +
  geom_point(shape = 22, size = 3)

## EXPORT ######################################################################
# X ----------------------------------------------------------------------------
startR::lazy_ggsave(
  plot = p,
  filename = "fig3_spatial_event_study",
  width = 15,
  height = 15
)
