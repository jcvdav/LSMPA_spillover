all: dag figures
figures: results/img/fig3_effects_by_mpa_and_spp.pdf
analysis: regressions
regressions: data/output/main_reg.rds data/output/gear_mpa_regs.rds data/output/gear_spp_regs.rds
panels: data/processed/annual_full_estimation_panel.rds
clean_rfmo_data: data/processed/rfmo_iattc_tuna_monthly_gear_flag.rds data/processed/rfmo_wcpfc_tuna_quarterly_gear_flag.rds data/processed/rfmo_iotc_tuna_monthly_gear_flag.rds data/processed/rfmo_iccat_tuna_annual_gear_flag.rds
dag: workflow.png

## STEP 1 -  CLEANING DATA #####################################################

# RFMO data --------------------------------------------------------------------
# Clean IATTC
data/processed/rfmo_iattc_tuna_monthly_gear_flag.rds: scripts/01_cleaning/01_clean_iattc.R data/raw/RFMO_data/IATTC/PublicPSTuna/PublicPSTunaFlag.csv data/raw/RFMO_data/IATTC/PublicLLTunaBillfish/PublicLLTunaBillfishMt.csv
		cd $(<D);Rscript $(<F)

# Clean WCPFC
data/processed/rfmo_wcpfc_tuna_quarterly_gear_flag.rds: scripts/01_cleaning/02_clean_wcpfc.R data/raw/RFMO_data/WCPFC/WCPFC_S_PUBLIC_BY_1x1_QTR_FLAG_3/WCPFC_S_PUBLIC_BY_1x1_QTR_FLAG.csv data/raw/RFMO_data/WCPFC/WCPFC_L_PUBLIC_BY_FLAG_YR_7/WCPFC_L_PUBLIC_BY_FLAG_YR.csv
		cd $(<D);Rscript $(<F)

# Clean IOTC
data/processed/rfmo_iotc_tuna_monthly_gear_flag.rds: scripts/01_cleaning/03_clean_iotc.R data/raw/RFMO_data/IOTC/IOTC-DATASETS-2023-04-24-CE-ALL_1950-2021/IOTC-DATASETS-2023-04-24-CE-Surface_1950-2021.csv data/raw/RFMO_data/IOTC/IOTC-DATASETS-2023-04-24-CE-ALL_1950-2021/IOTC-DATASETS-2023-04-24-CE-Longline_1950-2021.csv
		cd $(<D);Rscript $(<F)

# Clean ICCAT
data/processed/rfmo_iccat_tuna_annual_gear_flag.rds: scripts/01_cleaning/05_clean_iccat.R data/raw/RFMO_data/ICCAT/ICCAT_database.rds
		cd $(<D);Rscript $(<F)

data/raw/RFMO_data/ICCAT/ICCAT_database.rds: scripts/01_cleaning/04_convert_iccat_mdb_to_rds.R data/raw/RFMO_data/ICCAT/t2ce_20230131web.mdb
		cd $(<D);Rscript $(<F)

# MPA data --------------------------------------------------------------------
# Clean LMPA data set
data/processed/clean_lmpas.gpkg: scripts/01_cleaning/06_clean_lmpas.R
		cd $(<D);Rscript $(<F)


## Step 2 - MAKE DATA SETT #####################################################

# Combine all clean RFMO data into an annual panel
data/processed/rfmo_all_annual_gear_flag.rds: scripts/02_make_data/01_combine_all_rfmos_into_annual.R data/processed/rfmo_iattc_tuna_monthly_gear_flag.rds data/processed/rfmo_wcpfc_tuna_quarterly_gear_flag.rds data/processed/rfmo_iotc_tuna_monthly_gear_flag.rds data/processed/rfmo_iccat_tuna_annual_gear_flag.rds
		cd $(<D);Rscript $(<F)

# Build distance grid
data/processed/distance_grid.rds: scripts/02_make_data/02_build_distance_grid.R data/processed/rfmo_all_annual_gear_flag.rds
		cd $(<D);Rscript $(<F)

# Build annual panel combining RFMO data and distance grid
data/processed/annual_panel.rds: scripts/02_make_data/03_build_annual_panel.R data/processed/distance_grid.rds data/processed/rfmo_all_annual_gear_flag.rds
		cd $(<D);Rscript $(<F)

# Build estimation panels. Note two targets in a single rule
data/processed/annual_full_estimation_panel.rds data/processed/annual_relevant_mpa_gears_estimation_panel.rds: scripts/02_make_data/04_build_estimation_panels.R data/processed/annual_panel.rds
		cd $(<D);Rscript $(<F)


## STEP 3 -  ANALYSIS ##########################################################

# CPUE analysis ----------------------------------------------------------------
# Effect of MPA on CPUE
data/output/main_reg.rds: scripts/03_analysis/01_main_regs.R data/processed/annual_full_estimation_panel.rds data/processed/annual_relevant_mpa_gears_estimation_panel.rds
		cd $(<D);Rscript $(<F)

# Effect on CPUE by MPA and species -  note that it's two targets
data/output/gear_mpa_regs.rds data/output/gear_spp_regs.rds: scripts/03_analysis/02_regs_by_mpa_and_spp.R data/processed/annual_relevant_mpa_gears_estimation_panel.rds
		cd $(<D);Rscript $(<F)

## CONTENT #####################################################################

# Figures ----------------------------------------------------------------------

# Panel of effect by MPA and species
results/img/fig3_effects_by_mpa_and_spp.pdf results/img/fig3_effects_by_mpa_and_spp.png: scripts/04_content/fig3_effects_by_mpa_and_spp.R data/output/gear_mpa_regs.rds data/output/gear_spp_regs.rds
		cd $(<D);Rscript $(<F)

# Tables -----------------------------------------------------------------------

## OTHERS ######################################################################

# Draw makefile dag
workflow.png: Makefile
	make -Bnd | make2graph -b | dot -Tpng -Gdpi=300 -o workflow.png
