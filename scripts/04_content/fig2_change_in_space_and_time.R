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
  tidyverse
)

# Source custom funcions -------------------------------------------------------
source(here("scripts/00_set_up.R"))

# Load data --------------------------------------------------------------------
most_relevant_panel <- readRDS(file = here("data", "processed", "annual_relevant_mpa_gears_estimation_panel.rds"))

baci_plot <- function(data, type = "pts"){
  # browser()

  gear <- unique(data$gear)
  col <- c(unname(gear_palette[gear]), "gray")
  # shape <- ifelse(gear == "purse_seine", 21, 22)

  plot <- data %>%
    mutate(timing = ifelse(post == 0, "Before", "After"),
           timing = fct_relevel(timing, "Before", "After"),
           treatment = ifelse(near == 1, "Near", "Far"),
           treatment = fct_relevel(treatment, "Near", "Far")) %>%
    ggplot(mapping = aes(x = timing, y = cpue,
                         group = treatment,
                         fill = treatment,
                         color = treatment,
                         shape = gear)) +
    scale_color_manual(values = col) +
    scale_fill_manual(values = col) +
    scale_shape_manual(values = gear_shapes) +
    guides(shape = "none",
           color = "none",
           fill = guide_legend(override.aes = list(shape = 22, size = 1))) +
    theme(legend.position = "None") +
    labs(y = "CPUE",
         x = "Period")

  if(type == "pts") {
    plot <- plot +
      stat_summary(geom = "line",
                   fun = "mean",
                   linetype = "dashed",
                   position = position_dodge(width = 0.5)) +
      # stat_summary(geom = "errorbar",
      #              fun.data = "mean_se",
      #              alpha = 1,
      #              width = 0.1,
      #              position = position_dodge(width = 0.5)) +
      # stat_summary(geom = "point",
      #              fun = "mean",
      #              size = 4,
      #              color = "black",
      #              position = position_dodge(width = 0.5))
    stat_summary(geom = "pointrange",
                 fun.data = "mean_se",
                 fatten = 8,
                 color = "black",
                 position = position_dodge(width = 0.5))
  }

  if(type == "cols") {
    plot <- plot +
      stat_summary(geom = "col", fun = "mean", position = "dodge") +
      stat_summary(geom = "linerange", fun.data = "mean_se", position = position_dodge(width = 1),
                   color = "black")
  }

  return(plot)

}

## PROCESSING ##################################################################

## VISUALIZE ###################################################################

# BACI plots (Panels A-C) ------------------------------------------------------
# Panel A - All LL data
ll_baci_plot <- most_relevant_panel %>%
  filter(gear == "longline") %>%
  rename(cpue = cpue_tot) %>%
  mutate(cpue = cpue * 1e3) %>%
  baci_plot() +
  guides(color = "none") +
  theme(legend.position = c(0, 1),
        legend.justification = c(0, 1)) +
  labs(y = "CPUE (MT / 1000 hooks)",
       title = "All longine",
       fill = "Distance")

# Motu motira relevan example
ll_baci_motu <- most_relevant_panel %>%
  filter(gear == "longline",
         wdpaid == "555543712") %>%
  rename(cpue = cpue_tot) %>%
  mutate(cpue = cpue * 1e3) %>%
  baci_plot() +
  labs(y = "CPUE (MT / 1000 hooks)",
       title = "Longline (Motu Motiro Hiva)",
       fill = "Distance",
       color = "Distance")

# Purse seine example
ps_baci_pipa <- most_relevant_panel %>%
  filter(gear == "purse_seine",
         wdpaid == "309888") %>%
  rename(cpue = cpue_tot) %>%
  mutate(cpue = cpue) %>%
  baci_plot() +
  labs(y = "CPUE (MT / Set)",
       title = "Purse seine (PIPA)",
       fill = "Distance",
       color = "Distance")

# CPUE plots (Panelas A - C) ---------------------------------------------------
# X ----------------------------------------------------------------------------
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
# Panel A - All purse seine data within 200 NM
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
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 100, linetype = "dashed") +
  geom_smooth(method = "loess", span = 0.9,
              fill = unname(gear_palette)[1],
              color = unname(gear_palette)[1]) +
  geom_point(shape = 21, size = 2,
             fill = "#0A3161",
             color = "black") +
  scale_size_continuous(labels = scales::percent) +
  labs(x = "Dist. from LMPA (naut. miles)",
       y = "Change in CPUE (MT / Set)",
       title = "All purse seine")

# Panel B - Example of specific PS data
ps_delta_cpue_dist_plot <- most_relevant_panel %>%
  filter(wdpaid %in% c(
    # "11753", #Galapagos
    # "309888", #PIPA
    # "555512151" # Chagos
    "400011_B" #PRI (Jarvis)
    # "555629385" # Revilla
  ),
  gear == "purse_seine") %>%
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
  geom_hline(yintercept = 0,
             linetype = "dashed") +
  geom_vline(xintercept = 100,
             linetype = "dashed") +
  geom_smooth(method = "loess",
              span = 1,
              fill = "#0A3161",
              color = "#0A3161") +
  geom_point(shape = 21,
             size = 1,
             fill = "#0A3161",
             color = "black") +
  labs(x = "Dist. from LMPA   (naut. miles)",
       y = "Change in CPUE",
       color = "LMPA",
       title = "Purse seine (PRI [Jarvis])") +
  theme(legend.position = c(1, 1),
        legend.justification = c(1, 1),
        legend.title = element_blank())

ps_delta_cpue_dist_plot

# Panel B- Example of specific longline data
ll_delta_cpue_dist_plot <- most_relevant_panel %>%
  filter(wdpaid %in% c(
    "220201" # Papahanaumokuakea
    # "400011_B" #PRI (Wake)
    # "555556875_A"# Coral sea
    # "555543712" # Motu Motiro Hiva
  ),
  gear == "longline",
  between(event, -10, 10)) %>%
  group_by(id, wdpaid, short_name, dist, post) %>%
  summarize(effort = sum(effort) / 1000,
            tot_mt = sum(tot_mt),
            .groups = "drop") %>%
  ungroup() %>%
  mutate(
    cpue = tot_mt / effort,
    dist = (floor(dist / 75) * 75) + 37.5,
    dist_f = as.factor(-1 * dist)) %>%
  group_by(post, dist, short_name) %>%
  summarize(cpue = mean(cpue, na.rm = T), .groups = "drop") %>%
  ungroup() %>%
  pivot_wider(names_from = post,
              values_from = cpue,
              names_prefix = "cpue_") %>%
  mutate(delta = cpue_1 - cpue_0,
         pct_change = delta / cpue_0) %>%
  ggplot(aes(x = dist,
             y = delta,
             group = short_name)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 300, linetype = "dashed") +
  geom_smooth(method = "loess", span = 1) +
  geom_point(shape = 22, size = 1, color = "black") +
  geom_smooth(method = "loess",
              span = 1,
              fill = "#024731",
              color = "#024731") +
  geom_point(shape = 21,
             size = 1,
             fill = "#024731",
             color = "black") +
  guides(color = guide_legend(ncol = 1,
                              override.aes = list(fill = "transparent")),
         fill = "none") +
  labs(x = "Dist. from LMPA   (naut. miles)",
       y = "Change in CPUE",
       color = "LMPA",
       title = "Longline (Papahanaumokuakea)") +
  theme(legend.position = c(1, 1),
        legend.justification = c(1, 1),
        legend.title = element_blank())

# Combine all panels -----------------------------------------------------------
# bottom left side of plot (B-C)
cpue_time_subplot <- plot_grid(ll_baci_motu,
                               ps_baci_pipa,
                               ncol = 2,
                               labels = c("B", "C"))

# bottom ight side of plot (E-F)
cpue_dist_subplot <- plot_grid(ll_delta_cpue_dist_plot,
                               ps_delta_cpue_dist_plot,
                               ncol = 2,
                               labels = c("E", "F"))

## VISUALIZE ###################################################################

p <- plot_grid(ll_baci_plot, all_ps_delta_cpue_dist_plot,
               cpue_time_subplot, cpue_dist_subplot,
               ncol = 2,
               labels = c("A", "D"),
               rel_heights = c(2,1))

startR::lazy_ggsave(
  plot = p,
  filename = "fig2_visual_change",
  width = 20,
  height = 16
)

