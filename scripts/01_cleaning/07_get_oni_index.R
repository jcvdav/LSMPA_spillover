################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# List of indices available at: https://psl.noaa.gov/data/climateindices/list/
#
# Information on ONI
# Oceanic Niño Index: From NOAA Climate Prediction Center (CPC)
# Three month running mean of NOAA ERSST.V5 SST anomalies in the Niño 3.4 region
# (5N-5S, 120-170W), based on changing base period which consist of multiple
# centered 30-year base periods. These 30-year base periods will be used to
# calculate the anomalies for successive 5-year periods in the historical record.
#
# Data are read from https://psl.noaa.gov/data/correlation/oni.data
#
# Description provided in original data file
# ONI from CPC
# Provided by NOAA/PSL
# From http://www.cpc.ncep.noaa.gov/data/indices/oni.ascii.txt
# As of 09/20/2015, uses NOAA ERSST V5 from V3
# represents 3 month running mean of ERSST.v5 SST
# anomalies in the NiÃ±o 3.4 region
# (5N-5oS, 120o-170oW)]
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  tidyverse
)

# Load data --------------------------------------------------------------------
oni <- data.table::fread("https://psl.noaa.gov/data/correlation/oni.data",
                         skip = 1,
                         col.names = c("year", 1:12),
                         fill = T) %>%
  head(-9)

## PROCESSING ##################################################################
monthly_oni <- oni %>%
  magrittr::set_colnames(c("year", 1:12)) %>%
  mutate_all(as.numeric) %>%
  filter(year <= 2022) %>%
  pivot_longer(cols = c(2:13), names_to = "month", values_to = "oni") %>%
  mutate(qtr = lubridate::quarter(lubridate::ym(paste(year, month, sep = "/"))))

# Get annual means -------------------------------------------------------------
quarterly_oni <- monthly_oni %>%
  group_by(year, qtr) %>%
  summarize(oni_avg = mean(oni),
            oni_sd = sd(oni),
            oni_M = max(oni),
            oni_m = min(oni))

annual_oni <- monthly_oni %>%
  group_by(year) %>%
  summarize(oni_avg = mean(oni),
            oni_sd = sd(oni),
            oni_M = max(oni),
            oni_m = min(oni))

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
saveRDS(object = monthly_oni,
        file = here("data", "raw", "oni", "monthly_oni.rds"))

saveRDS(object = quarterly_oni,
        file = here("data", "raw", "oni", "quarterly_oni.rds"))

saveRDS(object = annual_oni,
        file = here("data", "raw", "oni", "annual_oni.rds"))
