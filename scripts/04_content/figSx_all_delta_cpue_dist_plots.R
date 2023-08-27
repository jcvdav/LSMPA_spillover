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

# Load data --------------------------------------------------------------------

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
#################
cpue_dist_plot <- function(mpa, spp, data, gear) {
  # browser()

  cpue <- parse(text = paste0("cpue_", spp))
  max <- ifelse(gear == "purse_seine", 200, 600)

  plot <- NULL

  check <- data %>%
    filter(wdpaid == mpa) %>%
    select(id, lat, lon, post, wdpaid, event, dist, near, contains(spp)) %>%
    drop_na(near) %>%
    count(near, post)

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

## VISUALIZE ###################################################################
# Change in CPUE in time
most_relevant_panel %>%
  filter(gear == "longline") %>%
  select(wdpaid, event, near, effort, tot_mt) %>%
  group_by(wdpaid, event, near) %>%
  summarize(effort = sum(effort),
            tot_mt = sum(tot_mt)) %>%
  ungroup() %>%
  mutate(cpue = tot_mt / effort) %>%
  select(wdpaid, event, near, cpue) %>%
  pivot_wider(names_from = near,
              values_from = cpue,
              names_prefix = "cpue_") %>%
  mutate(cpue = cpue_1 - cpue_0,
         pct = cpue / cpue_0) %>%
  ggplot(aes(x = event, y = pct)) +
  # geom_point() +
  geom_smooth(span = 0.5)

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
