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
  cowplot,
  kableExtra,
  panelsummary,
  fixest,
  here,
  tidyverse
)

# Load data --------------------------------------------------------------------
annual_panel <- readRDS(file = here("data", "processed", "annual_full_estimation_panel.rds"))
most_relevant_panel <- readRDS(file = here("data", "processed", "annual_relevant_mpa_gears_estimation_panel.rds"))


# ## PROCESSING ##################################################################
main_reg <- feols(log(cpue_tot) ~ i(post, near, 0) | id + year + effort_measure,
                  panel.id = ~id + year,
                  vcov = function(x)vcov_conley_hac(x, id = ~id,
                                                    time = ~year,
                                                    lat = ~lat,
                                                    lon = ~lon,
                                                    cutoff = 200,
                                                    lag = 5),
                  fsplit = ~nice_gear,
                  data = annual_panel)

# MPA-level regressions
relevant_mpa_gear_reg <- feols(log(cpue_tot) ~ i(post, near, 0) | id + year + effort_measure,
                               panel.id = ~id + year,
                               vcov = function(x)vcov_conley_hac(x, id = ~id,
                                                                 time = ~year,
                                                                 lat = ~lat,
                                                                 lon = ~lon,
                                                                 cutoff = 200,
                                                                 lag = 5),
                               fsplit = ~nice_gear,
                               data = most_relevant_panel)

# Build regression table -------------------------------------------------------
panelsummary(main_reg,
             relevant_mpa_gear_reg,
             colnames = c("", "Combined", "Purse Seine", "Longline"),
             panel_labels = c("Panel A: All data", "Panel B: Relevant MPA-gear combinations"),
             stars = "econ",
             pretty_num = T,
             collapse_fe = T,
             coef_map = c("post::1:near" = "Post x Near"),
             gof_omit = "With|IC|RMSE|Std.|effort",
             hline_after = T,
             format = "latex",
             caption = "Spillover effects of 13 Larg Marine Protected Areas on catch-per-unit effort of tuna fisheries (Yellowfin tuna, Bigeye tuna, Albacore tuna). Coefficients are difference-in-difference estimates for change in CPUE. The first column combines purse seine and longline data. The second column presents models fit to purse seine data, and the third column presents models fit to longline data only.") %>%
  footnote(list("* p < 0.1, ** p < 0.05, *** p < 0.01",
                "Numbers in parenthesis are standard errors robust to heteroskedasticity",
                "and spatio-temporal autocorrelation (200 km cutoff; 5 yr lag).",
                "When the data used contains more than one effort unit",
                "(i.e. days, sets, hooks) we include fixed-effects",
                "These preliminary estimates that exclude Pitcairn data.")) %>%
  cat(file = here("results", "tab", "main_regression_table.tex"))


# Extract gear reference points -------------------------------------------------
gear_stats <- relevant_mpa_gear_reg %>%
  map_dfr(broom::tidy, .id = "sample") %>%
  mutate(sample = str_remove(sample, ".+; sample: "),
         gear = str_sub(sample, 1, 2),
         mpa = str_remove(sample, "LL |PS "),
         mpa = fct_reorder(mpa, estimate, max),
         gear = fct_relevel(gear, "PS", "LL"),
         gear = ifelse(gear == "PS", "Purse seine", "Longline"),
         gear = fct_relevel(gear, "Purse seine", "Longline")) %>%
  filter(!sample == "Full sample")



## Regressions by MPA and Species ##############################################

# MPA-level analysis -----------------------------------------------------------
# Fit models
relevant_by_mpa <- feols(log(cpue_tot) ~ i(post, near, 0) | id + year,
                         panel.id = ~id + year,
                         vcov = function(x)vcov_conley_hac(x, id = ~id,
                                                           time = ~year,
                                                           lat = ~lat,
                                                           lon = ~lon,
                                                           cutoff = 200,
                                                           lag = 5),
                         split = ~paste(nice_gear, short_name),
                         data = most_relevant_panel)

# Extract coefficients into a data.frame
relevant_by_mpa_df <- relevant_by_mpa %>%
  map_dfr(broom::tidy, .id = "sample") %>%
  mutate(sample = str_remove(sample, ".+; sample: "),
         gear = str_sub(sample, 1, 2),
         mpa = str_remove(sample, "LL |PS "),
         # mpa = fct_relevel(mpa, ""),
         gear = ifelse(gear == "PS", "Purse seine", "Longline"),
         gear = fct_relevel(gear, "Purse seine", "Longline"),
         spp = "All that apply")

# Buid plot
# Still ahev to add a nice SVGN figure of PS / LL vessels
p1 <- ggplot(data = relevant_by_mpa_df,
             mapping = aes(y = mpa, x = estimate, shape = gear)) +
  geom_rect(data = gear_stats,
            mapping = aes(xmin = estimate - std.error,
                          xmax = estimate + std.error,
                          ymin = -Inf,
                          ymax = Inf),
            fill = "gray90",
            alpha = 0.5,
            inherit.aes = F) +
  geom_vline(data = gear_stats,
             mapping = aes(xintercept = estimate)) +
  geom_vline(xintercept =  0,
             linetype = "dashed") +
  geom_pointrange(aes(xmin = estimate -std.error,
                      xmax = estimate + std.error),
                  fill = "black", fatten = 6) +
  scale_shape_manual(values = gear_shapes) +
  facet_wrap(~gear, ncol = 1, scales = "free_y") +
  labs(x = "Effect on CPUE") +
  theme(legend.position = "None",
        axis.title.y = element_blank())

# ggplot(data = relevant_by_mpa_df,
#        mapping = aes(x = gear, y = estimate, fill = mpa)) +
#   geom_hline(yintercept =  0, linetype = "dashed") +
#   geom_pointrange(aes(ymin = estimate -std.error,
#                       ymax = estimate + std.error),
#                   shape = 21,
#                   fatten = 6,
#                   position = position_dodge(width = 0.5)) +
#   scale_fill_manual(values = mpa_palette) +
#   labs(x = "Fishing Gear",
#        y = "Effect on CPUE",
#        fill = "MPA")
#
# p1 <- ggplot(data = relevant_by_mpa_df,
#        mapping = aes(x = mpa, y = estimate, shape = gear, fill = gear)) +
#   geom_hline(yintercept =  0,
#              linetype = "dashed") +
#   geom_pointrange(aes(ymin = estimate -std.error,
#                       ymax = estimate + std.error),
#                   fatten = 6,
#                   position = position_dodge(width = 0.5)) +
#   scale_fill_manual(values = gear_palette) +
#   scale_shape_manual(values = gear_shapes) +
#   scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
#   labs(x = "",
#        y = "Effect on CPUE",
#        fill = "Species") +
#   theme(legend.position = "None",
#         axis.title.x = element_blank())


########

panel_for_spp_regs <- most_relevant_panel %>%
  mutate(cpue_skj = ifelse(gear == "longline", 0, cpue_skj)) %>%
  select(wdpaid, short_name, year, event, id, lat, lon, nice_gear, flag, effort, near, post, cpue_alb, cpue_bet, cpue_skj, cpue_yft, -cpue_tot) %>%
  pivot_longer(cols = contains("cpue"), names_to = "spp", values_to = "cpue_tot") %>%
  filter(cpue_tot > 0) %>%
  drop_na(cpue_tot)

gear_spp_regs <- feols(log(cpue_tot) ~ i(post, near, 0) | id + year,
                       panel.id = ~id + event,
                       vcov = function(x)vcov_conley_hac(x, id = ~id,
                                                         time = ~year,
                                                         lat = ~lat,
                                                         lon = ~lon,
                                                         cutoff = 200,
                                                         lag = 5),
                       data = panel_for_spp_regs,
                       split = ~paste(nice_gear, spp))


gear_spp_df <- gear_spp_regs %>%
  map_dfr(broom::tidy, .id = "sample") %>%
  drop_na(estimate) %>%
  mutate(sample = str_remove(sample, ".+; sample: "),
         gear = str_sub(sample, 1, 2),
         spp = str_extract(sample, "cpue_.+"),
         spp = str_to_upper(str_remove(spp, "cpue_"))) %>%
  mutate(gear = ifelse(gear == "PS", "Purse seine", "Longline"),
         gear = fct_relevel(gear, "Purse seine", "Longline"),
         spp = fct_relevel(spp, "ALB", "SKJ", "BET", "YFT"))

#Add a nice legend of colors by fish species, with little figures
p2 <- ggplot(gear_spp_df, aes(x = estimate, y = spp, fill = spp, shape = gear)) +
  geom_rect(data = gear_stats,
            mapping = aes(xmin = estimate - std.error,
                          xmax = estimate + std.error,
                          ymin = -Inf,
                          ymax = Inf),
            fill = "gray90",
            alpha = 0.5,
            inherit.aes = F) +
  geom_vline(data = gear_stats,
             mapping = aes(xintercept = estimate)) +
  geom_vline(xintercept =  0,
             linetype = "dashed") +
  geom_pointrange(aes(xmin = estimate -std.error,
                      xmax = estimate + std.error),
                  fatten = 6) +
  scale_shape_manual(values = gear_shapes) +
  scale_fill_manual(values = tuna_palette) +
  guides(shape = guide_legend(title = "Gear",
                              order = 1,
                              override.aes = list(size = 1, fill = "black")),
         fill = guide_legend(title = "Species",
                             order = 2,
                             override.aes = list(shape = 21, size = 1))) +
  facet_wrap(~gear, ncol = 1, scales = "free_y") +
  labs(x = "Effect on CPUE") +
  theme(axis.title.y = element_blank(),
        legend.position = "bottom",
        legend.box = "vertical")

# ggplot(gear_spp_df, aes(x = gear, y = estimate, fill = spp, shape = gear)) +
#   geom_hline(yintercept =  0, linetype = "dashed") +
#   geom_pointrange(aes(ymin = estimate -std.error,
#                       ymax = estimate + std.error),
#                   fatten = 6,
#                   position = position_dodge(width = 0.5)) +
#   scale_fill_manual(values = tuna_palette) +
#   scale_shape_manual(values = gear_shapes) +
#   guides(fill = guide_legend(override.aes = list(shape = 21))) +
#   labs(x = "",
#        y = "Effect on CPUE",
#        fill = "Species",
#        shape = "Fishing Gear") +
#   theme(axis.title.x = element_blank(), legend.box = "horizontal")
#




leg <- get_legend(p2)

p3 <- plot_grid(p1,
                p2 + theme(legend.position = "None"),
                labels = c("AUTO"),
                align = "hv",
                ncol = 1)

p <- plot_grid(p3, leg,
               ncol = 1,
               rel_heights = c(6, 1))

startR::lazy_ggsave(plot = p,
                    filename = "fig3_effects_by_mpa_and_spp",
                    width = 10,
                    height = 20)

