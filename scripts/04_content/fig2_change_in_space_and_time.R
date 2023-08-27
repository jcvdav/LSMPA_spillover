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

  plot <- data %>%
    mutate(timing = ifelse(post == 0, "Before", "After"),
           timing = fct_relevel(timing, "Before", "After"),
           treatment = ifelse(near == 1, "Near", "Far"),
           treatment = fct_relevel(treatment, "Near", "Far")) %>%
    ggplot(mapping = aes(x = timing, y = cpue,
                         fill = treatment,
                         color = treatment,
                         group = treatment)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_color_manual(values = dist_palette) +
    scale_fill_manual(values = dist_palette) +
    guides(fill = guide_legend(ncol = 1),
           color = guide_legend(ncol = 1)) +
    theme(legend.position = "None",
          axis.title.x = element_blank()) +
    labs(y = "CPUE")

  if(type == "pts") {
    plot <- plot +
      stat_summary(geom = "line", aes(color = treatment),
                   fun = "mean",
                   linetype = "dashed",
                   position = position_dodge(width = 0.5)) +
      stat_summary(geom = "pointrange",
                   fun.data = "mean_se",
                   shape = ifelse(unique(data$gear == "purse_seine"), 21, 22),
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
         dist = (floor(dist / 10) *10) + 5,
         dist_f = as.factor(-1 * dist))

## VISUALIZE ###################################################################

# CPUE plots (Panelas A - C) ---------------------------------------------------
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
              fill = "#DCE1E5",
              color = "#0A3161") +
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
    "11753", #Galapagos
    # "309888", #PIPA
    "555512151" # Chagos
    # "400011_B", #PRI (Jarvis)
    # "555629385" # Revilla
    ),
    gear == "purse_seine") %>%
  group_by(id, wdpaid, short_name, dist, lat, lon, post) %>%
  summarize(effort = sum(effort),
            tot_mt = sum(skj_mt),
            .groups = "drop") %>%
  ungroup() %>%
  mutate(cpue = tot_mt / effort,
         dist = (floor(dist / 10) *10) + 5,
         dist_f = as.factor(-1 * dist)) %>%
  group_by(post, dist, short_name) %>%
  summarize(cpue = mean(cpue, na.rm = T), .groups = "drop") %>%
  ungroup() %>%
  pivot_wider(names_from = post,
              values_from = cpue, names_prefix = "cpue_") %>%
  mutate(delta = cpue_1 - cpue_0,
         pct_change = delta / cpue_0) %>%
  ggplot(aes(x = dist, y = delta, fill = short_name, color = short_name, group = short_name)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 100, linetype = "dashed") +
  geom_smooth(method = "loess", span = 1) +
  geom_point(shape = 21, size = 1, color = "black") +
  scale_color_manual(values = c("#B2B2B2", "#024731")) +
  scale_fill_manual(values = c("#B2B2B2", "#024731")) +
  guides(fill = guide_legend(ncol = 1),
         color = guide_legend(ncol = 1)) +
  labs(x = "Dist. from LMPA   (naut. miles)",
       y = "Change in CPUE",
       fill = "LMPA",
       color = "LMPA",
       title = "Purse seine") +
  theme(legend.position = "top",
        legend.title = element_blank())

# Panel B- Example of specific longline data
ll_delta_cpue_dist_plot <- most_relevant_panel %>%
  filter(wdpaid %in% c(
    "220201", # Papahanaumokuakea
    "400011_B" #PRI (Wake)
    # "555543712" # Motu Motiro Hiva
  ),
  gear == "longline",
         between(event, -10, 10)) %>%
  group_by(id, wdpaid, short_name, dist, post) %>%
  summarize(effort = sum(effort) / 1000,
            tot_mt = sum(yft_mt),
            .groups = "drop") %>%
  ungroup() %>%
  mutate(
    cpue = tot_mt / effort,
    dist = (floor(dist / 10) *10) + 5,
    dist_f = as.factor(-1 * dist)) %>%
  group_by(post, dist, short_name) %>%
  summarize(cpue = mean(cpue, na.rm = T), .groups = "drop") %>%
  ungroup() %>%
  pivot_wider(names_from = post,
              values_from = cpue,
              names_prefix = "cpue_") %>%
  mutate(delta = cpue_1 - cpue_0,
         pct_change = delta / cpue_0) %>%
  ggplot(aes(x = dist, y = delta, fill = short_name, color = short_name, group = short_name)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 300, linetype = "dashed") +
  geom_smooth(method = "loess", span = 1) +
  geom_point(shape = 22, size = 1, color = "black") +
  scale_color_manual(values = c("#024731", "#B2B2B2")) +
  scale_fill_manual(values = c("#024731", "#B2B2B2")) +
  guides(fill = guide_legend(ncol = 1),
         color = guide_legend(ncol = 1)) +
  labs(x = "Dist. from LMPA   (naut. miles)",
       y = "Change in CPUE",
       fill = "LMPA",
       color = "LMPA",
       title = "Longline") +
  theme(legend.position = "top",
        legend.title = element_blank())

# BACI plots (Panels D-F) ------------------------------------------------------
# Panel D - All LL data
ll_baci_plot <- most_relevant_panel %>%
  filter(gear == "longline") %>%
  rename(cpue = cpue_tot) %>%
  mutate(cpue = cpue * 1e3) %>%
  baci_plot() +
  labs(y = "CPUE (MT / 1000 hooks)",
       title = "All longine")

# Motu motira relevan example
ll_baci_motu <- most_relevant_panel %>%
  filter(gear == "longline",
         wdpaid == "555543712") %>%
  rename(cpue = cpue_tot) %>%
  mutate(cpue = cpue * 1e3) %>%
  baci_plot() +
  theme(legend.position = "top",
        legend.title = element_blank()) +
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
  theme(legend.position = "top",
        legend.title = element_blank()) +
  labs(y = "CPUE (MT / Set)",
       title = "Purse seine (Phoenix Islands)",
       fill = "Distance",
       color = "Distance")

# Combine all panels -----------------------------------------------------------
# bottom left side of plot (B-C)
cpue_dist_subplot <- plot_grid(ps_delta_cpue_dist_plot,
                               ll_delta_cpue_dist_plot,
                               ncol = 2,
                               labels = c("B", "C"))
# bottom ight side of plot (E-F)
cpue_time_subplot <- plot_grid(ps_baci_pipa, ll_baci_motu,
                               ncol = 2,
                               labels = c("E", "F"))

## VISUALIZE ###################################################################

p <- plot_grid(all_ps_delta_cpue_dist_plot, ll_baci_plot,
               cpue_dist_subplot, cpue_time_subplot, ncol = 2,
               labels = c("A", "D"))

startR::lazy_ggsave(
  plot = p,
  filename = "fig2_visual_change",
  width = 18,
  height = 16
)
