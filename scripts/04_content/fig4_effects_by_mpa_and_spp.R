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

# Source custom functions ------------------------------------------------------
source(here("scripts/00_set_up.R"))

# Load data --------------------------------------------------------------------
# Data to add general model fits in the background of plots
gear_stats <- readRDS(file = here("data", "output", "relevant_mpa_gear_combination_model_coefs.rds"))

# Now load the model objects (they are multifit or lists)
# For MPA analysis
gear_mpa_regs <- readRDS(file = here("data", "output", "gear_mpa_regs.rds"))

# And now spp models
gear_spp_regs <- readRDS(file = here("data", "output", "gear_spp_regs.rds"))


## PROCESSING ##################################################################
# Extract coefficients into a data.frame ---------------------------------------
# For MPA analysis
relevant_by_mpa_df <- extract_mpa_coefs(gear_mpa_regs)                          # With fixed effects

# For Species analysis
gear_spp_df <- extract_spp_coefs(gear_spp_regs)                                 # With fixed effects

## VISUALIZE ###################################################################
# Build panel A) MPA-level coefficients ----------------------------------------
labeler <- function(x) {
  str_remove(x, "PS |LL ") %>%
    str_replace_all("ā", "a")
}
panel_A <- ggplot(data = relevant_by_mpa_df,
                  mapping = aes(y = sample,
                                x = estimate,
                                shape = gear,
                                fill = gear)) +
  geom_rect(data = gear_stats,
            mapping = aes(xmin = conf.low,
                          xmax = conf.high,
                          ymin = -Inf,
                          ymax = Inf),
            fill = "gray90",
            alpha = 0.5,
            inherit.aes = F) +
  geom_vline(data = gear_stats,
             mapping = aes(xintercept = estimate),
             linetype = "dotted") +
  geom_vline(xintercept =  0) +
  geom_pointrange(aes(xmin = conf.low,
                      xmax = conf.high),
                  color = "black",
                  fatten = 6) +
  scale_shape_manual(values = gear_shapes) +
  scale_fill_manual(values = gear_palette) +
  facet_wrap(~gear, ncol = 1, scales = "free_y") +
  labs(x = "Effect on CPUE") +
  theme(legend.position = "None",
        axis.title.y = element_blank()) +
  scale_y_discrete(labels = labeler)

# Build panel B) Species-level coefficients ------------------------------------
labeler <- function(x) {
  str_to_upper(str_remove_all(x, "PS |LL |cpue_"))
}
panel_B <- ggplot(data = gear_spp_df,
                  mapping = aes(x = estimate,
                                y = sample,
                                fill = spp,
                                shape = gear)) +
  geom_rect(data = gear_stats,
            mapping = aes(xmin = conf.low,
                          xmax = conf.high,
                          ymin = -Inf,
                          ymax = Inf),
            fill = "gray90",
            alpha = 0.5,
            inherit.aes = F) +
  geom_vline(data = gear_stats,
             mapping = aes(xintercept = estimate),
             linetype = "dotted") +
  geom_vline(xintercept =  0) +
  geom_pointrange(aes(xmin = conf.low,
                      xmax = conf.high),
                  fatten = 6) +
  scale_shape_manual(values = gear_shapes) +
  scale_fill_manual(values = tuna_palette) +
  guides(shape = guide_legend(title = "Gear",
                              order = 1,
                              override.aes = list(size = 1, fill = gear_palette[1:2])),
         fill = guide_legend(title = "Species",
                             order = 2,
                             override.aes = list(shape = 21, size = 1))) +
  facet_wrap(~gear, ncol = 1, scales = "free_y") +
  labs(x = "Effect on CPUE") +
  theme(axis.title.y = element_blank(),
        legend.position = "bottom",
        legend.box = "horizontal") +
  scale_y_discrete(labels = labeler)

# Build the plot ---------------------------------------------------------------
# Combine both panels
panels <- plot_grid(panel_A,
                    panel_B + theme(legend.position = "None"),
                    labels = c("AUTO"),
                    align = "vh",
                    ncol = 2)

# Add the legend at the bottom
final_plot <- plot_grid(panels, get_legend(panel_B),
                        ncol = 1,
                        rel_heights = c(8, 1))

## EXPORT ######################################################################
# Figure 3 for main text -------------------------------------------------------
startR::lazy_ggsave(plot = final_plot,
                    filename = "fig4_effects_by_mpa_and_spp",
                    width = 18,
                    height = 10)

