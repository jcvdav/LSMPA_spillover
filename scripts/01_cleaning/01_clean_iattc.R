################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Description - From PDF that came with the data
#
# For PS Tuna ------------------------------------------------------------------
# Data fields / Campos de datos
# Year year / año
# Month month / mes
# <Flag> country / pabellón
# <SetType> set type / tipo de lance
# DEL: dolphin set / lance sobre delfines
# NOA: unassociated tuna set / lance sobre atunes no asociados
# OBJ: floating-object set / lance sobre objeto flotante
# LatC1 latitude of the center of a 1°x1° cell, negative values south of the equator / latitud del centro de una celda de 1°x1°, valores negativos al sur del ecuador
# LonC1 longitude of the center of a 1°x1° cell / longitud del centro de una celda de 1°x1°
# NumSets number of sets / número de lances
# Col 7-14 retained catch of the indicated species, in metric tons / captura retenida de la especie indicada, en toneladas
#
#FAO ASFIS species codes / Códigos de especies FAO ASFIS
# English Español Scientific/Científico
# ALB Albacore Albacora (atún blanco) Thunnus alalunga
# BET Bigeye Patudo (ojo grande) Thunnus obesus
# BKJ Black skipjack Barrilete negro Euthynnus lineatus
# BZX E. Pacific/striped bonito Bonito Sarda chiliensis, S. orientalis
# PBF Pacific bluefin Aleta azul del Pacífico Thunnus orientalis
# SKJ Skipjack Barrilete Katsuwonus pelamis
# TUN Tunas, nei* Atunes, nep* Thunnini
# YFT Yellowfin Aleta amarilla Thunnus albacares
#
# For LL -----------------------------------------------------------------------
# Data fields / Campos de datos
# Year year / año
# Month month / mes
# Flag country / pabellón
# LatC5 latitude of the center of the 5°x5° cell, negative values south of the equator / latitud del centro de una celda de 5°x5°, valores negativos al sur de la línea ecuatorial
# LonC5 longitude of the center of 5°x5° cell / longitud del centro de una celda de 5°x5°
# Hooks number of hooks / número de anzuelos
# <Spp>n number of individuals of the indicated species / número de ejemplares de la especie indicada
# <Spp>mt weight of the indicated species, in metric tons / peso de la especie indicada, en toneladas
#
# FAO ASFIS species codes / Códigos de especies FAO ASFIS
# English Español Scientific/Científico
# ALB Albacore Albacora Thunnus alalunga
# BET Bigeye Patudo (ojo grande) Thunnus obesus
# PBF Pacific bluefin Aleta azul del Pacífico Thunnus orientalis
# SKJ Skipjack Barrilete Katsuwonus pelamis
# TUN1 Tunas, nei* Atunes, nep* Thunnini
# YFT Yellowfin Aleta amarilla Thunnus albacares
# BIL Marlin, sailfish, spearfish Aguja, marlín, pez vela Istiophoridae, Xiphiidae
# BLM Black marlin Marlín aguja negra Istiompax indica
# BUM Blue marlin Marlín aguja azul Makaira nigricans
# MLS Striped marlin Marlín rayado Kajikia audax
# SFA Indo-Pacific sailfish Pez vela Istiophorus platypterus
# SSP Shortbill spearfish Marlín trompa corta Tetrapturus angustirostris
# SWO Swordfish Pez espada Xiphias gladius
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
pacman::p_load(here,
               janitor,
               tidyverse)

# Source custom funcions -------------------------------------------------------
source(here("scripts/00_set_up.R"))

# Load data --------------------------------------------------------------------
ps_tuna <-
  read_csv(file = here(
    "data",
    "raw",
    "RFMO_data",
    "IATTC",
    "PublicPSTuna",
    "PublicPSTunaFlag.csv"
  )) %>%
  clean_names()

ll_tuna <-
  read_csv(
    file = here(
      "data",
      "raw",
      "RFMO_data",
      "IATTC",
      "PublicLLTunaBillfish",
      "PublicLLTunaBillfishMt.csv"
    )
  ) %>%
  clean_names()

## PROCESSING ##################################################################

# Clean purse seine data -------------------------------------------------------
ps_tuna_clean <- ps_tuna %>%
  rename(
    lat = lat_c1,
    lon = lon_c1,
    effort = num_sets,
    # Tunas - Organized from most to least important
    skj_mt = skj,
    yft_mt = yft,
    bet_mt = bet,
    pbf_mt = pbf
    #
    # Removing albacore because there were only 22 MT caught since 2011 (~0.0003%)
    # alb_mt = alb,
    #
    #Not tunas
    # bkj_mt = bkj,
    # bzx_mt = bzx,
    # tun_mt = tun,
  ) %>%
  mutate(tot_mt = skj_mt + yft_mt + bet_mt + pbf_mt) %>%
  filter(tot_mt > 0) %>%  # Remove records that had effort and catch data for a species other than tuna
  filter(effort > 0) %>%  # And  records that report effort = 0
  mutate(
    cpue_skj = skj_mt / effort,
    cpue_yft = yft_mt / effort,
    cpue_bet = bet_mt / effort,
    cpue_pbf = pbf_mt / effort,
    cpue_tot = tot_mt / effort
  ) %>%
  mutate(effort_measure = "sets",
         gear = "purse_seine",
         grid = "1x1",
         rfmo = "iattc")

# Clean longline data ----------------------------------------------------------
ll_tuna_clean <- ll_tuna %>%
  rename(lat = lat_c5, lon = lon_c5) %>%
  select(year, month, flag, lat, lon, hooks, contains("mt")) %>%
  rename(
    effort = hooks,
    # Tunas - Organized from most to least important
    alb_mt = al_bmt,
    bet_mt = be_tmt,
    yft_mt = yf_tmt
    ## Removing PBF and SKJ because 1992 aer neglegible (~<1%)
    # pbf_mt = pb_fmt
    # skj_mt = sk_jmt
  ) %>%
  mutate(tot_mt = alb_mt + bet_mt + yft_mt) %>%
  filter(tot_mt > 0) %>%  # Remove records that had effort and catch data for a species other than tuna
  filter(effort > 0) %>%  # And  records that report effort = 0
  mutate(
    cpue_alb = alb_mt / effort,
    cpue_bet = bet_mt / effort,
    cpue_yft = yft_mt / effort,
    cpue_tot = tot_mt / effort
  ) %>%
  mutate(effort_measure = "hooks",
         gear = "longline",
         grid = "5x5",
         rfmo = "iattc")

# Combine ----------------------------------------------------------------------
iattc_tuna <-
  bind_rows(ps_tuna_clean, ll_tuna_clean) %>%
  select(
    rfmo,
    year,
    month,
    gear,
    flag,
    grid,
    lat,
    lon,
    effort,
    effort_measure,
    contains("_mt"),
    contains("cpue_")
  )

# Make sure we have the right species
check_mt(iattc_tuna, cutoff = 1992)

# Make sure gear-effort untis are correct
check_effort_gear(iattc_tuna)

# Check for 0s and NAs
test(iattc_tuna)

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
saveRDS(
  object = iattc_tuna,
  file = here("data", "processed", "rfmo_iattc_tuna_monthly_gear_flag.rds")
)



