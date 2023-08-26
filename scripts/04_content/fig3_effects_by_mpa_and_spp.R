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

# User-defined functions -------------------------------------------------------
# A function to extract moedl info for gear-mpa models
extract_mpa_coefs <- function(model) {

  has_fes <- ifelse(length(model[[1]]$fixef_vars) > 1 ,
                    "With FEs",
                    "Without FEs")
  model %>%
    map_dfr(broom::tidy, .id = "sample") %>%
    filter(str_detect(term, ":")) %>%
    mutate(sample = str_remove(sample, ".+; sample: "),
           sample = fct_reorder(sample, estimate),
           gear = str_sub(sample, 1, 2),
           mpa = str_remove(sample, "LL |PS "),
           gear = ifelse(gear == "PS", "Purse seine", "Longline"),
           gear = fct_relevel(gear, "Purse seine", "Longline"),
           mpa = fct_relevel(mpa,
                             "Revillagigedo",
                             "Galápagos",
                             "PIPA",
                             "Papahānaumokuākea",
                             "PRI (Wake)",
                             "PRI (Jarvis)",
                             "Chagos",
                             "Motu Motiro Hiva",
                             "Coral Sea",
                             "Nazca-Desventuradas"),
           model = has_fes)
}

# A funciton to extract model info for gear-spp models
extract_spp_coefs <- function(model) {

  has_fes <- ifelse(length(model[[1]]$fixef_vars) > 1 ,
                    "With FEs",
                    "Without FEs")

  model %>%
    map_dfr(broom::tidy, .id = "sample") %>%
    filter(str_detect(term, ":")) %>%
    mutate(sample = str_remove(sample, ".+; sample: "),
           sample = fct_reorder(sample, estimate),
           gear = str_sub(sample, 1, 2),
           spp = str_extract(sample, "cpue_.+"),
           spp = str_to_upper(str_remove(spp, "cpue_"))) %>%
    mutate(gear = ifelse(gear == "PS", "Purse seine", "Longline"),
           gear = fct_relevel(gear, "Purse seine", "Longline"),
           spp = fct_relevel(spp,
                             "YFT",
                             "SKJ",
                             "BET",
                             "ALB",
                             ),
           model = has_fes)
}

# Load data --------------------------------------------------------------------
# Data to add general model fits in the background of plots
gear_stats <- readRDS(file = here("data", "output", "relevant_mpa_gear_combination_model_coefs.rds"))

# Now load the model objects (they are multifit or lists)
# For MPA analysis
gear_mpa_regs <- readRDS(file = here("data", "output", "gear_mpa_regs.rds"))
gear_mpa_regs_wo_fe <- readRDS(file = here("data", "output", "gear_mpa_regs_wo_fe.rds"))

# And now spp models
gear_spp_regs <- readRDS(file = here("data", "output", "gear_spp_regs.rds"))
gear_spp_regs_wo_fe <- readRDS(file = here("data", "output", "gear_spp_regs_wo_fe.rds"))


## PROCESSING ##################################################################
# Extract coefficients into a data.frame ---------------------------------------
# For MPA analysis
relevant_by_mpa_df <- extract_mpa_coefs(gear_mpa_regs)                          # With fixed effects
relevant_by_mpa_df_wo_fe <- extract_mpa_coefs(gear_mpa_regs_wo_fe)              # Without fixed effects

# For Species analysis
gear_spp_df <- extract_spp_coefs(gear_spp_regs)                                 # With fixed effects
gear_spp_df_wo_fe <- extract_spp_coefs(gear_spp_regs_wo_fe)                     # Without fixed effects

## VISUALIZE ###################################################################
# Build panel A) MPA-level coefficients ----------------------------------------
labeler <- function(x) {
  str_remove(x, "PS |LL ")
}
panel_A <- ggplot(data = relevant_by_mpa_df,
                  mapping = aes(y = sample,
                                x = estimate,
                                shape = gear)) +
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


# Supplementary figures comparing results with and without FEs -----------------
# Form MPA-level analysis
# Important points:
# - Including FEs does not change results for Galapagos, pIPA, Revilla or Papahanaumokuakea, Jarvis PS, Wake LL, Motu
# - It flips the results for Chagos LL and PS, Jarvis LL, and Nazca
mpa_compare_models <- bind_rows(relevant_by_mpa_df, relevant_by_mpa_df_wo_fe) %>%
  mutate(mpa_gear = paste(mpa, gear)) %>%
  ggplot(aes(x = gear, y = estimate, shape = gear, fill = model)) +
  geom_pointrange(aes(ymin = estimate - std.error,
                      ymax = estimate + std.error),
                  position = position_dodge(width = 0.5)) +
  coord_flip() +
  scale_shape_manual(values = gear_shapes) +
  scale_fill_manual(values = fe_palette) +
  guides(fill = guide_legend(override.aes = list(shape = 21))) +
  geom_hline(yintercept = 0) +
  facet_wrap(~mpa, ncol = 4) +
  theme(legend.position = c(1, 0),
        legend.justification = c(1, 0),
        legend.box = "horizontal") +
  labs(y = "Effect on CPUE",
       shape = "Gear",
       fill = "Model")

# For species-level analysis
spp_compare_models <- bind_rows(gear_spp_df, gear_spp_df_wo_fe) %>%
  ggplot(aes(x = spp, y = estimate, shape = gear, fill = model)) +
  geom_pointrange(aes(ymin = estimate - std.error,
                      ymax = estimate + std.error),
                  position = position_dodge(width = 0.5)) +
  coord_flip() +
  scale_shape_manual(values = gear_shapes) +
  scale_fill_manual(values = fe_palette) +
  guides(fill = guide_legend(override.aes = list(shape = 21))) +
  geom_hline(yintercept = 0) +
  facet_wrap(~gear) +
  labs(y = "Effect on CPUE",
       shape = "Gear",
       fill = "Model")

## EXPORT ######################################################################
# Figure 3 for main text -------------------------------------------------------
startR::lazy_ggsave(plot = final_plot,
                    filename = "fig3_effects_by_mpa_and_spp",
                    width = 12,
                    height = 9)
# Supplementary figures --------------------------------------------------------
# Comparing FE and WOFE for MPAs
startR::lazy_ggsave(plot = mpa_compare_models,
                    filename = "figSX_effects_by_mpa_with_and_without_fe",
                    width = 15,
                    height = 10)
# Comparing FE and WOFE for spp
startR::lazy_ggsave(plot = spp_compare_models,
                    filename = "figSX_effects_by_spp_with_and_without_fe",
                    width = 12,
                    height = 8)

