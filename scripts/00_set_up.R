# Function for spatial_hac SEs ------------------------------------------------
# From: https://github.com/lrberge/fixest/issues/350#issuecomment-1671226930
vcov_conley_hac <- function(x, id, time, lat, lon, cutoff, lag) {
  # Spatial portion
  vcov_conley <-
    fixest::vcov_conley(
      x = x,
      lat = lat,
      lon = lon,
      cutoff = cutoff,
      distance = "spherical")

  # Panel portion
  vcov_hac <-
    fixest::vcov_DK(
      x = x,
      # unit = id,
      time = time,
      lag = lag)
  # Heteroskedasticity
  vcov_robust <-
    fixest::vcov_cluster(
      x = x,
      cluster = id)


  vcov_conley_hac <- vcov_conley +
    vcov_hac -
    vcov_robust

  if(any(diag(vcov_conley_hac) < 0)){
    # We 'fix' it
    all_attr <- attributes(vcov_conley_hac)
    vcov_conley_hac <- fixest:::mat_posdef_fix(vcov_conley_hac)
    attributes(vcov_conley_hac) <- all_attr
    message("Variance contained negative values in the diagonal and was 'fixed' (a la Cameron, Gelbach & Miller 2011).")
  }

  return(vcov_conley_hac)
}

# Formula for an inverse-hiperbolic sinte ransformation ------------------------
ihs <- function(x){
  log(x + sqrt((x ^ 2) + 1))
}

# Set a global theme -----------------------------------------------------------
ggplot2::theme_set(
  ggplot2::theme_bw()
)
ggplot2::theme_update(
  panel.grid.major = ggplot2::element_blank(),
  line = ggplot2::element_line(color = "black",
                      linewidth = 0.176389),
  panel.grid.minor = ggplot2::element_blank(),
  legend.background = ggplot2::element_blank(),
  legend.key = ggplot2::element_blank(),
  strip.background = ggplot2::element_blank(),
  strip.text = ggplot2::element_text(hjust = 0),
  text = ggplot2::element_text(size = 8),
)


# Testing function -------------------------------------------------------------
test <- function(data) {
  c(
    "cpue_na" = sum(is.na(data$cpue_tot)),
    "tot_mt_0" = sum(data$tot_mt == 0),
    "effort_0" = sum(data$effort == 0)
  )
}

# Different color palettes -----------------------------------------------------

# From IPCC color palettes
blues <- dplyr::tribble(~"R", ~"G", ~"B",
                        239, 243, 255,
                        189, 215, 231,
                        107, 174, 214,
                        49, 130, 189,
                        8, 81, 156)

blues <- grDevices::rgb(red = blues$R,
                        green = blues$G,
                        blue = blues$B,
                        maxColorValue = 256)


tuna_palette <- c("YFT" = "#FEBC11",
                  "BET" = "#003660",
                  "SKJ" = "#111517",
                  "ALB" = "#DCE1E5",
                  "All" = "transparent")

gear_palette <- c("PS" = "#08519B",
                  "LL" = "#006D2C",
                  "Purse seine" = "#08519B",
                  "Longline" = "#006D2C",
                  "purse_seine" = "#08519B",
                  "longline" = "#006D2C",
                  "All" = "black")

gear_shapes <- c("PS" = 21,
                 "LL" = 22,
                 "Purse seine" = 21,
                 "Longline" = 22,
                 "purse_seine" = 21,
                 "longline" = 22,
                 "All" = 24)

fe_palette <- c("With FEs" = "black",
                "Without FEs" = "gray")

dist_alpha <- c("Near" = 1,
                "Far" = 0.5,
                "near" = 1,
                "far" = 0.5)

check_mt <- function(data, cutoff = 0) {
  data %>%
    filter(year >= cutoff) %>%
    select(gear, contains("mt"), -tot_mt) %>%
    group_by(gear) %>%
    summarize_all(sum, na.rm = T)
}

check_mt_e <- function(data, cutoff = 0) {
  data %>%
    filter(year >= cutoff) %>%
    select(gear, contains("mt"), effort) %>%
    group_by(gear) %>%
    summarize_all(sum, na.rm = T)
}

check_effort_gear <- function(data) {
  data %>%
    count(gear, effort_measure)
}

# A function to extract moedl info for gear-mpa models
extract_mpa_coefs <- function(model) {

  has_fes <- ifelse(length(model[[1]]$fixef_vars) > 1 ,
                    "With FEs",
                    "Without FEs")
  model %>%
    map_dfr(broom::tidy, .id = "sample", conf.int = TRUE, conf.level = 0.95) %>%
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
    map_dfr(broom::tidy, .id = "sample", conf.int = TRUE, conf.level = 0.95) %>%
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
                             "ALB"),
           model = has_fes)
}

# Plotting functions -----------------------------------------------------------
baci_plot <- function(data, type = "pts"){

  gear <- unique(data$gear)
  col <- c(unname(gear_palette[gear]), "gray")

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
    scale_y_continuous(expand = expansion(mult = c(0, 0.1)),
                       limits = c(0, NA)) +
    guides(shape = "none",
           color = "none",
           fill = guide_legend(override.aes = list(shape = gear_shapes[gear], size = 1))) +
    theme(legend.position = "None") +
    labs(y = "CPUE",
         x = "Period")

  if(type == "pts") {
    plot <- plot +
      stat_summary(geom = "line",
                   fun = "mean",
                   linetype = "dashed",
                   arrow = grid::arrow(angle = 25,
                                       length = unit(0.15, "inches"),
                                       type = "closed"),
                   position = position_dodge(width = 0.5)) +
      stat_summary(geom = "pointrange",
                   fun.data = "mean_se",
                   fatten = 6,
                   color = "black",
                   position = position_dodge(width = 0.5))
  }

  if(type == "cols") {
    plot <- plot +
      stat_summary(geom = "col", fun = "mean", position = "dodge") +
      stat_summary(geom = "linerange",
                   fun.data = "mean_se",
                   position = position_dodge(width = 1),
                   color = "black")
  }

  return(plot)

}

# Function to calculat ethe percent change
delta_cpue <- function(data){
  data %>%
    group_by(post, near) %>%
    summarize(cpue = mean(cpue)) %>%
    pivot_wider(names_from = post,
                values_from = cpue,
                names_prefix = "post_") %>%
    mutate(change = ((post_1 - post_0) / post_0) * 100)
}
