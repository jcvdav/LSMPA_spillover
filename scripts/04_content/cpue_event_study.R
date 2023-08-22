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
annual_panel <- readRDS(here("data", "processed", "annual_panel.rds")) %>%
  filter(effort_measure %in% c("sets", "hooks"),
         between(event, -10, 10),
         !wdpaid == "555624172")

###
ns_per_period_ps <- annual_panel %>%
  filter((gear == "purse_seine" & !is.na(near_100))) %>%
  group_by(wdpaid, rfmo, name, gear, post, near_100) %>%
  summarize(n_pixels = n_distinct(id)) %>%
  group_by(wdpaid, gear) %>%
  mutate(nn = n(),
         nnn = sum(n_pixels))

ns_per_period_ll <- annual_panel %>%
  filter((gear == "longline" & !is.na(near_300))) %>%
  group_by(wdpaid, rfmo, name, gear, post, near_300) %>%
  summarize(n_pixels = n_distinct(id)) %>%
  group_by(wdpaid, gear) %>%
  mutate(nn = n(),
         nnn = sum(n_pixels))

ns_per_period <- bind_rows(ns_per_period_ps,
                           ns_per_period_ll)

# MPA-gear combinations without enough observations ----------------------------
not_enough <- ns_per_period %>%
  filter(nn <= 3 | n_pixels <= 5) %>%
  select(wdpaid, name, gear, rfmo, nnn) %>%
  distinct()

# MPA-gear combinations with enough observations -------------------------------
enough <- filter(ns_per_period, nn >= 4 & n_pixels >= 5) %>%
  select(wdpaid, name, gear,rfmo, nnn) %>%
  distinct()

keep <- enough %>%
  select(wdpaid, gear) %>%
  distinct() %>% mutate(keep = paste(wdpaid, gear)) %>%
  pull(keep)

annual_panel <- annual_panel %>%
  filter(paste(wdpaid, gear) %in% keep)

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
  labs(x = "Distance from MPA boundary (nautical miles)",
       y = "Change in CPUE (MT / Set)",
       title = "LMPA: all | Gear: purse seines | Species: all") +
  guides(size = guide_legend(ncol = 2)) +
  theme_bw() +
  theme(legend.position = c(1, 1),
        legend.justification = c(1, 1),
        legend.background = element_blank())

ps_delta_cpue_dist_plot_gal <- annual_panel %>%
  filter(wdpaid == "11753",
         gear == "purse_seine",
         between(event, -10, 10),
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
  labs(x = "Distance from MPA boundary (nautical miles)",
       y = "Change in CPUE (MT / Set)",
       title = "LMPA: Galápagos Marine Reserve | Gear: purse seine | Species: Skipjack") +
  guides(size = guide_legend(ncol = 2)) +
  theme_bw() +
  theme(legend.position = c(1, 1),
        legend.justification = c(1, 1),
        legend.background = element_blank())

ll_delta_cpue_dist_plot_pri_wake <- annual_panel %>%
  filter(wdpaid == "400011_A",
         gear == "longline",
         between(event, -10, 10),
         !is.na(near_300)) %>%
  group_by(id, wdpaid, name, dist, lat, lon, event, near_300, post) %>%
  summarize(effort = sum(effort) / 100,
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
  geom_smooth(method = "loess", span = 0.9, fill = "#DCE1E5", color = "#003660", linetype = "dashed") +
  geom_point(shape = 21, size = 1, fill = "#003660") +  scale_size_continuous(labels = scales::percent) +
  labs(x = "Distance from MPA boundary (nautical miles)",
       y = "Change in CPUE (MT / 1000 hooks)",
       title = "LMPA: PRI (Wake) | Gear: longline | Species: Bigeye, Yellowfin, Albacore") +
  guides(size = guide_legend(ncol = 2)) +
  theme_bw() +
  theme(legend.position = c(1, 1),
        legend.justification = c(1, 1),
        legend.background = element_blank())


cpue_dist_subplot <- plot_grid(ps_delta_cpue_dist_plot_gal, ll_delta_cpue_dist_plot_pri_wake, ncol = 2)

plot_grid(ps_delta_cpue_dist_plot, ps_delta_cpue_dist_plot,
          cpue_dist_subplot, cpue_dist_subplot, ncol = 2, rel_heights = c(1.5, 1))



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
