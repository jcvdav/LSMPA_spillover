# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  Hmisc,
  tidyverse
)

# Load data --------------------------------------------------------------------
# Read the Microsoft access database
con <- mdb.get(file = here("data", "raw", "RFMO_data", "ICCAT", "t2ce_20230131web.mdb"))

# Export the data
saveRDS(object = con,
        file = here("data", "raw", "RFMO_data", "ICCAT", "ICCAT_database.rds"))
