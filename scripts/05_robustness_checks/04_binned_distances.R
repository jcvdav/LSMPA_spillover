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
                fml = log(cpue_tot) ~ i(dist_bin, "187.5") + post + i(dist_bin, post, "187.5") | flag ^ gear + wdpaid ^ gear ^ year,
                vcov = conley(cutoff = 200))


## VISUALIZE ###################################################################
p <- ps_mod %>%
  broom::tidy(conf.int = TRUE, conf.level = 0.95) %>%
  filter(str_detect(term, "[:digit:]:post")) %>%
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

# EXPORT #######################################################################
# Table

# Figure
startR::lazy_ggsave(
  plot = p,
  filename = "figS_binned_distances",
  width = 9,
  height = 5
)


