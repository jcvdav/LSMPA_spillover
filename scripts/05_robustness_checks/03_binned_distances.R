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
  fixest,
  tidyverse
)

# Source custom funcions -------------------------------------------------------
source(here("scripts/00_set_up.R"))

# Load data --------------------------------------------------------------------
most_relevant_panel <- readRDS(file = here("data", "processed", "annual_relevant_mpa_gears_estimation_panel.rds"))

## PROCESSING ##################################################################
# Purse seine models -----------------------------------------------------------
ps_mod <- feols(data = most_relevant_panel %>%
                  filter(gear == "purse_seine") %>%
                  mutate(dist_bin = (floor(dist / 25) * 25) + 12.5),
                fml = log(cpue_tot) ~ dist_bin + post + i(dist_bin, post, "187.5") | id + year + effort_measure,
                vcov = function(x)vcov_conley_hac(x, id = ~id,
                                                  time = ~year,
                                                  lat = ~lat,
                                                  lon = ~lon,
                                                  cutoff = 200,
                                                  lag = 5))

# Purse seine models -----------------------------------------------------------
ll_mod <- feols(data = most_relevant_panel %>%
                  filter(gear == "longline") %>%
                  mutate(dist_bin = (floor(dist / 75) * 75) + 37.5),
                fml = log(cpue_tot) ~ dist_bin + post + i(dist_bin, post, "562.5") | id + year,
                vcov = function(x)vcov_conley_hac(x, id = ~id,
                                                  time = ~year,
                                                  lat = ~lat,
                                                  lon = ~lon,
                                                  cutoff = 200,
                                                  lag = 5))


## VISUALIZE ###################################################################
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
               ncol = 1,
               labels = "auto")

# EXPORT #######################################################################
# Table

# Figure
startR::lazy_ggsave(
  plot = p,
  filename = "figS_binned_distances",
  width = 9,
  height = 10
)


