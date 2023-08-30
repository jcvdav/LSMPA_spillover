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
  broom,
  # ggiplot,
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
ps_plot <- ps_mod %>%
  tidy(conf.int = TRUE, conf.level = 0.95) %>%
  mutate(dist_bin = as.numeric(str_extract(term, "[:digit:]+"))) %>%
  bind_rows(tibble(dist_bin = 200, estimate = 0, std.error = 0)) %>%
  ggplot(aes(x = dist_bin, y = estimate)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  annotate(geom = "text", x = c(50, 150), y = 0.5, label = c("Near", "Far")) +
  geom_vline(xintercept = 100, linetype = "dotted") +
  geom_line(color = unname(gear_palette)[1],
            linetype = "dashed") +
  geom_pointrange(aes(ymin = estimate - std.error,
                      ymax = estimate + std.error),
                  fill = unname(gear_palette)[1],
                  shape = 21,
                  fatten = 8) +
  scale_x_continuous(limits = c(0, 200),
                     expand = expansion(0.01, 0)) +
  scale_y_continuous(limits = c(-0.15, 0.55),
                     expand = expansion(0.01, 0)) +
  labs(y = "Estimate and Conf. Int.",
       x = "Distance bin (NM)",
       title = NULL)


ll_plot <- ll_mod %>%
  tidy() %>%
  mutate(dist_bin = as.numeric(str_extract(term, "[:digit:]+"))) %>%
  bind_rows(tibble(dist_bin = 600, estimate = 0, std.error = 0)) %>%
  ggplot(aes(x = dist_bin, y = estimate)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  annotate(geom = "text", x = c(150, 450), y = 0.5, label = c("Near", "Far")) +
  geom_vline(xintercept = 300, linetype = "dotted") +
  geom_line(color = unname(gear_palette)[2],
            linetype = "dashed") +
  geom_pointrange(aes(ymin = estimate - std.error,
                      ymax = estimate + std.error),
                  fill = unname(gear_palette)[2],
                  shape = 22,
                  fatten = 8) +
  scale_x_continuous(limits = c(0, 600),
                     expand = expansion(0.01, 0)) +
  scale_y_continuous(limits = c(-0.15, 0.55),
                     expand = expansion(0.01, 0)) +
  labs(y = "Estimate and SE",
       x = "Distance bin (NM)",
       title = NULL)


p <- plot_grid(ps_plot,
               ll_plot,
               ncol = 2,
               labels = "AUTO")

## EXPORT ######################################################################
# X ----------------------------------------------------------------------------
startR::lazy_ggsave(
  plot = p,
  filename = "fig3_spatial_event_study",
  width = 18,
  height = 5
)

