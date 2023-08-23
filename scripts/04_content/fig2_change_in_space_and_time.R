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
  tidyverse
)

# Load data --------------------------------------------------------------------
annual_panel <- readRDS(file = here("data", "processed", "annual_full_estimation_panel.rds"))


baci_plot <- function(data, type = "pts", window = T, gear){

  if(window) {
    data <- data %>%
      filter(between(event, -10, 10))
  }

  plot <- data %>%
    mutate(timing = ifelse(post == 0, "Before", "After"),
           timing = fct_relevel(timing, "Before", "After"),
           treatment = ifelse(near == 1, "near", "far"),
           treatment = fct_relevel(treatment, "near", "far")) %>%
    ggplot(mapping = aes(x = timing, y = cpue,
                         fill = treatment,
                         color = treatment,
                         group = treatment)) +
    scale_color_brewer(palette = "Set1") +
    scale_fill_brewer(palette = "Set1") +
    theme_bw() +
    theme(legend.position = "None",
          axis.title.x = element_blank()) +
    labs(y = "CPUE")

  if(type == "pts") {
    plot <- plot +
      stat_summary(geom = "line", aes(color = treatment),
                   fun = "mean", linetype = "dashed") +
      stat_summary(geom = "pointrange",
                   fun.data = "mean_se",
                   shape = 21,
                   fatten = 6,
                   color = "black")
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
ps_delta_cpue_dist_data <- annual_panel %>%
  filter(gear == "purse_seine",
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


ps_delta_cpue_dist_plot <- ps_delta_cpue_dist_data %>%
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
  geom_smooth(method = "loess", span = 1, fill = "#DCE1E5", color = "#003660", linetype = "dashed") +
  geom_point(shape = 21, size = 2, fill = "#003660") +
  scale_size_continuous(labels = scales::percent) +
  labs(x = "Dist. from LMPA (naut. miles)",
       y = "Change in CPUE (MT / Set)") +
  guides(size = guide_legend(ncol = 2)) +
  theme_bw() +
  theme(legend.position = c(1, 1),
        legend.justification = c(1, 1),
        legend.background = element_blank())

ps_delta_cpue_dist_plot_gal <- annual_panel %>%
  filter(wdpaid == "11753",
         gear == "purse_seine",
         !is.na(near_100)) %>%
  group_by(id, wdpaid, name, dist, lat, lon, event, near_100, post) %>%
  summarize(effort = sum(effort),
            tot_mt = sum(skj_mt),
            .groups = "drop") %>%
  ungroup() %>%
  mutate(cpue = tot_mt / effort,
         dist = (floor(dist / 10) *10) + 5,
         dist_f = as.factor(-1 * dist)) %>%
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
  geom_smooth(method = "loess", span = 1, fill = "#DCE1E5", color = "#003660", linetype = "dashed") +
  geom_point(shape = 21, size = 2, fill = "#003660") +  scale_size_continuous(labels = scales::percent) +
  labs(x = "Dist. from LMPA (naut. miles)",
       y = "Change in CPUE") +
  guides(size = guide_legend(ncol = 2)) +
  theme_bw() +
  theme(legend.position = c(1, 1),
        legend.justification = c(1, 1),
        legend.background = element_blank())

ll_delta_cpue_dist_plot_pri_wake <- annual_panel %>%
  filter(wdpaid == "555512151",
         gear == "longline",
         between(event, -10, 10),
         !is.na(near_300)) %>%
  group_by(id, wdpaid, name, dist, lat, lon, event, near_300, post) %>%
  summarize(effort = sum(effort) / 1000,
            tot_mt = sum(tot_mt),
            .groups = "drop") %>%
  ungroup() %>%
  mutate(cpue = tot_mt / effort,
         dist = (floor(dist / 10) *10) + 5,
         dist_f = as.factor(-1 * dist)) %>%
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
  geom_vline(xintercept = 300, linetype = "dashed") +
  geom_smooth(method = "loess", span = 0.75, fill = "#DCE1E5", color = "gray", linetype = "dashed") +
  geom_point(shape = 21, size = 2, fill = "gray") +  scale_size_continuous(labels = scales::percent) +
  labs(x = "Dist. from LMPA (naut. miles)",
       y = "Change in CPUE") +
  guides(size = guide_legend(ncol = 2)) +
  theme_bw() +
  theme(legend.position = c(1, 1),
        legend.justification = c(1, 1),
        legend.background = element_blank())



# BACI PLOTS
ll_baci_plot <- most_relevant_panel %>%
  filter(gear == "longline") %>%
  rename(cpue = cpue_tot) %>%
  mutate(cpue = cpue * 1e3) %>%
  baci_plot(gear = "Longline")

ll_baci_motu <- most_relevant_panel %>%
  filter(gear == "longline",
         wdpaid == "555543712") %>%
  rename(cpue = cpue_tot) %>%
  mutate(cpue = cpue * 1e3) %>%
  baci_plot(gear = "Longline", window = 5)

ps_baci_pipa <- most_relevant_panel %>%
  filter(gear == "purse_seine",
         wdpaid == "309888") %>%
  rename(cpue = cpue_tot) %>%
  mutate(cpue = cpue) %>%
  baci_plot(gear = "Purse seine", window = 3)


cpue_dist_subplot <- plot_grid(ps_delta_cpue_dist_plot_gal,
                               ll_delta_cpue_dist_plot_pri_wake,
                               ncol = 2,
                               labels = c("B", "C"))

cpue_time_subplot <- plot_grid(ps_baci_pipa, ll_baci_motu,
                               ncol = 2,
                               labels = c("E", "F"))

## VISUALIZE ###################################################################

p <- plot_grid(ps_delta_cpue_dist_plot, ll_baci_plot,
               cpue_dist_subplot, cpue_time_subplot, ncol = 2,
               rel_heights = c(2, 1),
               labels = c("A", "D"))

startR::lazy_ggsave(
  plot = p,
  filename = "fig_2_visual_change",
  width = 24,
  height = 20
)
































#################
cpue_dist_plot <- function(mpa, spp, data, gear) {
  # browser()

  cpue <- parse(text = paste0("cpue_", spp))
  near <- ifelse(gear == "purse_seine", "near_100", "near_300")
  max <- ifelse(gear == "purse_seine", 200, 600)

  plot <- NULL

  check <- data %>%
    filter(wdpaid == mpa) %>%
    select(id, lat, lon, post, wdpaid, event, dist, near, contains(spp)) %>%
    drop_na(near) %>%
    count(!!sym(near), post)

  if(dim(check)[1] == 4) {

    plot_data <- data %>%
      filter(wdpaid == mpa) %>%
      select(id, lat, lon, post, wdpaid, event, dist, near, contains(spp)) %>%
      drop_na(near) %>%
      filter(eval(cpue) > 0) %>%
      mutate(dist = (floor(dist / 10) *10) + 5) %>%
      group_by(post, dist, eval(near)) %>%
      summarize(cpue = mean(eval(cpue), na.rm = T)) %>%
      ungroup() %>%
      pivot_wider(names_from = post,
                  values_from = cpue, names_prefix = "cpue_") %>%
      mutate(delta = cpue_1 - cpue_0) %>%
      drop_na(delta)

    if(dim(plot_data)[1] > 4) {
      name <- data %>%
        filter(wdpaid == mpa) %>%
        pull(name) %>%
        unique()

      plot <- ggplot(data = plot_data,
                     aes(x = dist, y = delta)) +
        geom_point() +
        geom_smooth(span = 1) +
        ggtitle(label = paste(name, spp, gear)) +
        scale_x_continuous(limits = c(0, max))
    }

  }


  return(plot)
}

safe <- safely(cpue_dist_plot)

mpa_spp_pairs <- annual_panel %>%
  filter((gear == "purse_seine" & !is.na(near_100) |
            gear == "longline" & !is.na(near_300))) %>%
  select(wdpaid, gear, contains("mt")) %>%
  pivot_longer(cols = contains("mt"), names_to = "spp", values_to = "mt") %>%
  mutate(spp = str_remove(spp, "_mt")) %>%
  filter(mt > 0) %>%
  group_by(wdpaid, gear, spp) %>%
  summarize(n = n(),
            mt = sum(mt))

mpa_spp_plots <- mpa_spp_pairs %>%
  mutate(cpue_dist_plot = pmap(list(mpa = wdpaid, spp = spp, gear = gear),
                               safe,
                               data = annual_panel)) %>%
  filter(map_lgl(cpue_dist_plot, function(x){!is.null(x$result)}))


mpa_spp_plots %>%
  mutate(title = paste0("results/img/cpue_dist_plots/", wdpaid, "_", gear, "_", spp, ".png")) %$%
  walk2(title, cpue_dist_plot, ~ggsave(plot = .y$result, filename = .x, width = 5, height = 5))

# Great:
# Gal, YFT, PS and LL
# Gal TOT LL and PS


# gal YFT; gal TOT;
# PIPA SKJ; PIPA tot;

# Flats
# PIPA YFT;
# Marianas trench


## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
