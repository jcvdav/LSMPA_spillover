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
most_relevant_panel <- readRDS(file = here("data", "processed", "annual_relevant_mpa_gears_estimation_panel.rds"))

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
  theme(legend.position = c(0, 0),
        legend.justification = c(0, 0)) +
  labs(y = "CPUE (MT / set)",
       title = "All purse seine",
       fill = "Distance") +
  annotate(geom = "text",
           x = c(1.25, 1.95),
           y = c(31, 25),
           label = c("Change in CPUE near: 22.85%",
                     "Change in CPUE far: 8.74%"),
           color = c(gear_palette["PS"], "gray50"))

# Panel B: Good example with a purse seine example
ps_good <- most_relevant_panel %>%
  filter(gear == "purse_seine",
         wdpaid == "309888") %>%
  rename(cpue = cpue_tot)

delta_cpue(ps_good)

ps_baci_good <- baci_plot(data = ps_good) +
  labs(y = "CPUE (MT / set)",
       title = "Phoenix Islands",
       fill = "Distance",
       color = "Distance") +
  annotate(geom = "text",
           x =c(1.75, 1.25),
           y = c(30, 45),
           label = c("1.93%",
                     "13.61%"),
           color = c("gray50", gear_palette["PS"]))

# Panel C: Bad example with purse seine data
ps_bad <- most_relevant_panel %>%
  filter(gear == "purse_seine",
         wdpaid == "555624169") %>%
  rename(cpue = cpue_tot)

delta_cpue(ps_bad)

ps_baci_bad <- baci_plot(data = ps_bad) +
  labs(y = "CPUE (MT / set)",
       title = "Nazca-Desventuradas",
       fill = "Distance",
       color = "Distance") +
  annotate(geom = "text",
           x =c(1.75, 1.25),
           y = c(35, 62),
           label = c("-16.9%",
                     "-0.8%"),
           color = c("gray50", gear_palette["PS"]))

# Panels E-H "spillover gradient" plots ----------------------------------------
# Now we build plots where we show the change in CPUE as one moves away from the
# MPA border. We also build one for PS, one for LL, and then one for a good and
# a bad example.

# Panel E - All purse seine data within 200 NM
ps_delta_cpue_dist_data <- most_relevant_panel %>%
  filter(!wdpaid == "555512151",
         gear == "purse_seine",
         between(event, -10, 10),
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
       y = "Change in CPUE (MT / set)",
       title = "All purse seine")

# Panel B - Example of specific PS data
ps_delta_cpue_dist_plot_good <- most_relevant_panel %>%
  filter(gear == "purse_seine",
         wdpaid == "309888") %>%
  group_by(id, wdpaid, short_name, dist, lat, lon, post) %>%
  summarize(effort = sum(effort),
            tot_mt = sum(tot_mt),
            .groups = "drop") %>%
  ungroup() %>%
  mutate(cpue = tot_mt / effort,
         dist = (floor(dist / 25) * 25) + 12.5,
         dist_f = as.factor(-1 * dist)) %>%
  group_by(post, dist, short_name) %>%
  summarize(cpue = mean(cpue, na.rm = T), .groups = "drop") %>%
  ungroup() %>%
  pivot_wider(names_from = post,
              values_from = cpue, names_prefix = "cpue_") %>%
  mutate(delta = cpue_1 - cpue_0,
         pct_change = delta / cpue_0) %>%
  ggplot(mapping = aes(x = dist,
                      y = delta,
                      group = short_name)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 100,
             linetype = "dotted") +
  annotate(geom = "text", x = c(50, 150), y = 17, label = c("Near", "Far")) +
  geom_smooth(method = "loess",
              span = 1,
              fill = "#08519B",
              color = "#08519B") +
  geom_point(shape = 21,
             size = 2,
             fill = "#08519B",
             color = "black") +
  labs(x = "Distance from border (NM)",
       y = "Change in CPUE",
       color = "LMPA",
       title = "Phoenix Islands") +
  theme(legend.position = c(1, 1),
        legend.justification = c(1, 1),
        legend.title = element_blank())

ps_delta_cpue_dist_plot_bad <- most_relevant_panel %>%
  filter(gear == "purse_seine",
         wdpaid == "400011_B") %>%
  group_by(id, wdpaid, short_name, dist, lat, lon, post) %>%
  summarize(effort = sum(effort),
            tot_mt = sum(tot_mt),
            .groups = "drop") %>%
  ungroup() %>%
  mutate(cpue = tot_mt / effort,
         dist = (floor(dist / 25) * 25) + 12.5,
         dist_f = as.factor(-1 * dist)) %>%
  group_by(post, dist, short_name) %>%
  summarize(cpue = mean(cpue, na.rm = T), .groups = "drop") %>%
  ungroup() %>%
  pivot_wider(names_from = post,
              values_from = cpue, names_prefix = "cpue_") %>%
  mutate(delta = cpue_1 - cpue_0,
         pct_change = delta / cpue_0) %>%
  ggplot(mapping = aes(x = dist,
                       y = delta,
                       group = short_name)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 100,
             linetype = "dotted") +
  annotate(geom = "text", x = c(50, 150), y = 20, label = c("Near", "Far")) +
  geom_smooth(method = "loess",
              span = 1,
              fill = "#08519B",
              color = "#08519B") +
  geom_point(shape = 21,
             size = 2,
             fill = "#08519B",
             color = "black") +
  labs(x = "Distance from border (NM)",
       y = "Change in CPUE",
       color = "LMPA",
       title = "PRI - Jarvis") +
  theme(legend.position = c(1, 1),
        legend.justification = c(1, 1),
        legend.title = element_blank())

# Combine all panels -----------------------------------------------------------
BACI_plots <- plot_grid(
  ps_baci_plot,
  plot_grid(ps_baci_good,
            ps_baci_bad,
            ncol = 2,
            labels = c("b", "c"),
            align = "hv"),
  ncol = 1,
  rel_heights = c(1.75, 1),
  labels = "a")

dist_plots <- plot_grid(
  all_ps_delta_cpue_dist_plot,
  plot_grid(ps_delta_cpue_dist_plot_good,
            ps_delta_cpue_dist_plot_bad,
            ncol = 2,
            labels = c("e", "f"),
            align = "hv"),
  ncol = 1,
  rel_heights = c(1.75, 1),
  labels = "d"
)

## VISUALIZE ###################################################################

p <- plot_grid(BACI_plots, dist_plots,
               ncol = 2)


startR::lazy_ggsave(
  plot = p,
  filename = "fig2_visual_change",
  width = 19,
  height = 14
)

