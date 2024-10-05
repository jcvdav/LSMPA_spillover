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

# Source custom functions ------------------------------------------------------
source(here("scripts/00_set_up.R"))

# Load data --------------------------------------------------------------------
# Model objects (they are multifit or lists)
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
                                shape = "PS",
                                fill = "PS")) +
  geom_vline(xintercept =  0) +
  geom_pointrange(aes(xmin = estimate - std.error,
                      xmax = estimate + std.error),
                  color = "black",
                  fatten = 6) +
  scale_shape_manual(values = gear_shapes) +
  scale_fill_manual(values = gear_palette) +
  labs(x = "Effect on CPUE") +
  theme(legend.position = "None",
        axis.title.y = element_blank(),
        strip.text = element_text(size = 10)) +
  scale_y_discrete(labels = labeler)

# Build panel B) Species-level coefficients ------------------------------------
labeler <- function(x) {
  y <- str_to_upper(str_remove_all(x, "PS |LL |cpue_"))
  z <- case_when(y == "YFT" ~ "Yellowfin",
                 y == "SKJ" ~ "Skipjack",
                 y == "BET" ~ "Bigeye",
                 y == "ALB" ~ "Albacore")
  return(z)
}

panel_B <- ggplot(data = gear_spp_df,
                  mapping = aes(x = estimate,
                                y = sample,
                                fill = spp)) +
  geom_vline(xintercept =  0) +
  geom_image(aes(x = estimate + (0.75 * std.error),
                 y = sample,
                 image = here("data", "raw", "gear_fish_pics", paste0(spp, ".svg"))),
             size = 0.35,
             nudge_y = 0.3) +
  geom_pointrange(aes(xmin = estimate - std.error,
                      xmax = estimate + std.error),
                  color = "black",
                  shape = 21,
                  fatten = 6) +
  scale_shape_manual(values = gear_shapes) +
  scale_fill_manual(values = tuna_palette) +
  scale_y_discrete(labels = labeler) +
  scale_x_continuous(expand = expansion(0.11, 0)) +
  labs(x = "Effect on CPUE",
       fill = "Species") +
  theme(axis.title.y = element_blank(),
        legend.position = "bottom",
        legend.box = "horizontal",
        strip.text = element_text(size = 10))

# Build the plot ---------------------------------------------------------------
# Combine both panels
final_plot <- plot_grid(panel_A,
                    panel_B + theme(legend.position = "None"),
                    labels = c("AUTO"),
                    align = "vh",
                    ncol = 2)

## EXPORT ######################################################################
# Figure 3 for main text -------------------------------------------------------
startR::lazy_ggsave(plot = final_plot,
                    filename = "fig4_effects_by_mpa_and_spp",
                    width = 18,
                    height = 7.5)

ggsave(plot = final_plot,
       filename = here("results", "img", "fig4_effects_by_mpa_and_spp.pdf"),
       device = cairo_pdf,
       width = 18,
       height = 7.5,
       units = "cm")

