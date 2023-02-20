# Main targets
data/processed/distance_grid.rds: scripts/02_make_data/02_build_distance_grid.R data/processed/clean_lmpas.gpkg data/processed/quarterly_all_rfmos.rds
				cd $(<D);Rscript $(<F)

## CLEANING RFMO DATA ##########################################################
# Combine into one
data/processed/quarterly_all_rfmos.rds: scripts/02_make_data/01_make_quarterly_all_rfmos.R data/processed/wcpfc_tuna_quarterly_gear_flag.rds data/processed/iattc_tuna_monthly_gear_flag.rds data/processed/iotc_tuna_monthly_gear_flag.rds data/processed/iccat_tuna_monthly_gear_flag.rds
				cd $(<D);Rscript $(<F)

# Clean IATTC
data/processed/iattc_tuna_monthly_gear_flag.rds: scripts/01_cleaning/01_clean_iattc.R
				cd $(<D);Rscript $(<F)

# Clean WCPFC
data/processed/wcpfc_tuna_quarterly_gear_flag.rds: scripts/01_cleaning/02_clean_wcpfc.R
				cd $(<D);Rscript $(<F)

# Clean IOTC
data/processed/iotc_tuna_monthly_gear_flag.rds: scripts/01_cleaning/03_clean_iotc.R
				cd $(<D);Rscript $(<F)

# Clean ICCAT
data/processed/iccat_tuna_monthly_gear_flag.rds: scripts/01_cleaning/05_clean_iccat.R data/raw/RFMO_data/ICCAT/ICCAT_database.rds
				cd $(<D);Rscript $(<F)

data/raw/RFMO_data/ICCAT/ICCAT_database.rds: scripts/01_cleaning/04_convert_iccat_mdb_to_rds.R
				cd $(<D);Rscript $(<F)

## CLEANING MPA DATA ###########################################################
data/processed/clean_lmpas.gpkg: scripts/01_cleaning/06_clean_lmpas.R
				cd $(<D);Rscript $(<F)

# draw makefile dag
workflow.png: Makefile
	make -Bnd | make2graph -b | dot -Tpng -Gdpi=300 -o workflow.png
