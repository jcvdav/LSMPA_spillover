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
  ggimage,
  tidyverse
)

# Source custom funcions -------------------------------------------------------
source(here("scripts/00_set_up.R"))

img <- function(pic) {here("data", "raw", "gear_fish_pics", paste0(pic, ".svg"))}

# Load data --------------------------------------------------------------------
most_relevant_panel <- readRDS(file = here("data", "processed", "annual_relevant_mpa_gears_estimation_panel.rds")) %>%
  filter(dist > 0)

## PROCESSING ##################################################################

## VISUALIZE ###################################################################

# BACI plots (Panels A-D) ------------------------------------------------------
# We first build BACI plots showing the mean CPUE for each of the before, after,
# control, impact groups. There is one main plot for each gear, and then a plot
# for a "good" and a "bad" example.

# Panel A - For all purse seine data
ps_data <- most_relevant_panel %>%
  filter(!wdpaid == "555512151", # Chagos is reported in different CPUE units
         gear == "purse_seine") %>%
  rename(cpue = cpue_tot)

delta_cpue(ps_data)

ps_baci_plot <- baci_plot(data = ps_data) +
  guides(color = "none") +
  theme(legend.position = "inside",
        legend.position.inside = c(0, 0),
        legend.justification = c(0, 0)) +
  labs(y = "CPUE (MT / set)",
       fill = "Distance") +
  annotate(geom = "text",
           x = c(1.25, 1.95),
           y = c(31, 25),
           label = c("Change in CPUE near: 23.85%",
                     "Change in CPUE far: 8.74%"),
           color = c(gear_palette["PS"], "gray50"))

# Panel B "spillover gradient" -------------------------------------------------
# Now we build plots where we show the change in CPUE as one moves away from the
# MPA border. We also build one for PS, one for LL, and then one for a good and
# a bad example.
ps_delta_cpue_dist_data <- most_relevant_panel %>%
  filter(!wdpaid == "555512151", # Chagos is reported in different CPUE units!wdpaid == "555512151",
         gear == "purse_seine",
         !is.na(near_100)) %>%
  group_by(id, wdpaid, name, dist, lat, lon, event, near_100, post) %>%
  summarize(effort = sum(effort),
            tot_mt = sum(tot_mt),
            .groups = "drop") %>%
  ungroup() %>%
  mutate(cpue = tot_mt / effort,
         dist = (floor(dist / 25) * 25) + 12.5,
         dist_f = as.factor(-1 * dist))

all_ps_delta_cpue_dist_plot <- ps_delta_cpue_dist_data %>%
  group_by(post, dist) %>%
  summarize(cpue = mean(cpue, na.rm = T), .groups = "drop") %>%
  ungroup() %>%
  pivot_wider(names_from = post,
              values_from = cpue, names_prefix = "cpue_") %>%
  mutate(delta = cpue_1 - cpue_0,
         pct_change = delta / cpue_0) %>%
  ggplot(aes(x = dist, y = delta)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 100, linetype = "dotted") +
  annotate(geom = "text", x = c(50, 150), y = 10, label = c("Near", "Far")) +
  geom_smooth(method = "loess", span = 0.9,
              fill = unname(gear_palette)[1],
              color = unname(gear_palette)[1]) +
  geom_point(shape = 21, size = 2,
             fill = "#08519B",
             color = "black") +
  scale_size_continuous(labels = scales::percent) +
  labs(x = "Distance from border (NM)",
       y = "Change in CPUE (MT / set)")

# Combine the panels -----------------------------------------------------------
p <- plot_grid(ps_baci_plot,
               all_ps_delta_cpue_dist_plot,
               labels = "auto")

## EXPORT ######################################################################
startR::lazy_ggsave(
  plot = p,
  filename = "fig2_visual_change",
  width = 19,
  height = 9
)

