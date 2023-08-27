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
  panel.grid.major = ggplot2::element_line(color = "black",
                                           linewidth = 0.1,
                                           linetype = "dashed"),
  panel.grid.minor = ggplot2::element_blank(),
  legend.background = ggplot2::element_blank(),
  legend.key = ggplot2::element_blank(),
  strip.background = ggplot2::element_blank(),
  strip.text = ggplot2::element_text(hjust = 0 ),
  text = ggplot2::element_text(size = 8),
  axis.text.y = ggplot2::element_text(size = 5)
)


# Testing function -------------------------------------------------------------
test <- function(data) {
  c(
    "cpue_na" = sum(is.na(data$cpue_tot)),
    "tot_mt_0" = sum(data$tot_mt == 0),
    "effort_0" = sum(data$effort == 0)
  )
}

tuna_palette <- c("YFT" = "#FEBC11",
                  "BET" = "#003660",
                  "SKJ" = "#111517",
                  "ALB" = "#DCE1E5",
                  "All" = "transparent")

mpa_palette <- c("Chagos" = "#047C91",
                 "Galápagos" = "#09847A",
                 "Motu Motiro Hiva" = "#6D7D33",
                 "Nazca-Desventuradas" = "#9CBEBE",
                 "Papahānaumokuākea" = "#0A3161",
                 "PIPA" = "#EF5645",
                 "PRI (Jarvis)" = "#DCD6CC",
                 "PRI (Wake)" = "#C9BF9D",
                 "Revillagigedo" = "#006341",
                 "All" = "transparent")

gear_palette <- c("PS" = "gray",
                 "LL" = "black",
                 "Purse seine" = "gray",
                 "Longline" = "black",
                 "All" = "transparent")

gear_shapes <- c("PS" = 21,
                 "LL" = 22,
                 "Purse seine" = 21,
                 "Longline" = 22,
                 "All" = 24)

fe_palette <- c("With FEs" = "black",
                "Without FEs" = "gray")

dist_palette <- c("Near" = "#F47321",
                  "Far" = "#257BB1",
                  "near" = "#F47321",
                  "far" = "#257BB1")

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
