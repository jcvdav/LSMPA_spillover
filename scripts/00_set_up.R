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
