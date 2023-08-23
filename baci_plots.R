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
      stat_summary(geom = "pointrange", fun.data = "mean_se", shape = 21, color = "black")
  }

  if(type == "cols") {
    plot <- plot +
      stat_summary(geom = "col", fun = "mean", position = "dodge") +
      stat_summary(geom = "linerange", fun.data = "mean_se", position = position_dodge(width = 1),
                   color = "black")
  }

  return(plot)

}

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------

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

plot_grid(ll_baci_plot,
          plot_grid(ps_baci_pipa, ll_baci_motu), ncol = 1)

# PS info
ps_baci_plot <- most_relevant_panel %>%
  filter(gear == "purse_seine") %>%
  rename(cpue = cpue_tot) %>%
  baci_plot(gear = "Purse seine")

ps_baci_plots <- ps_baci_plot +
  facet_wrap(~name, ncol = 2, scales = "free_y")

p1 <- plot_grid(ps_baci_plot, ps_baci_plots, ncol = 1)
p1


p2 <- plot_grid(ll_baci_plot +
                  theme(legend.position = c(0, 1),
                        legend.justification = c(0, 1),
                        legend.background = element_blank()),
                ll_baci_plots, ncol = 1)


p <- plot_grid(p1, p2, ncol = 2)

startR::lazy_ggsave(
  plot = p,
  filename = "fig_2_DID_bars",
  width = 16,
  height = 16)


## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
