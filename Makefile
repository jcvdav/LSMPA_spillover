all: dag main_figs supp_figs
main_figs: results/img/fig1_map.pdf results/img/fig2_visual_change.pdf results/img/fig3_effects_by_mpa_and_spp.pdf
supp_figs: results/img/figS1_map_of_overlaps.pdf results/img/figS2_mt_ts_overlaps.pdf results/img/figS3_partially_fully_covered_pts_map.pdf results/img/figS4_effects_by_mpa_with_and_without_fe.pdf results/img/figS5_effects_by_spp_with_and_without_fe.pdf
tables: results/tab/tab1_main_reg_table.tex results/tab/tabSx_main_regression_table.tex
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
data/processed/rfmo_wcpfc_tuna_quarterly_gear_flag.rds: scripts/01_cleaning/02_clean_wcpfc.R data/raw/RFMO_data/WCPFC/WCPFC_S_PUBLIC_BY_1x1_QTR_FLAG_3/WCPFC_S_PUBLIC_BY_1x1_QTR_FLAG.csv data/raw/RFMO_data/WCPFC/WCPFC_L_PUBLIC_BY_FLAG_QTR_8/WCPFC_L_PUBLIC_BY_FLAG_QTR.csv
		cd $(<D);Rscript $(<F)

# Clean IOTC
data/processed/rfmo_iotc_tuna_monthly_gear_flag.rds: scripts/01_cleaning/03_clean_iotc.R data/raw/RFMO_data/IOTC/IOTC-DATASETS-2023-04-24-CE-ALL_1950-2021/IOTC-DATASETS-2023-04-24-CE-Surface_1950-2021.csv data/raw/RFMO_data/IOTC/IOTC-DATASETS-2023-04-24-CE-ALL_1950-2021/IOTC-DATASETS-2023-04-24-CE-Longline_1950-2021.csv
		cd $(<D);Rscript $(<F)

# Clean ICCAT
data/raw/RFMO_data/ICCAT/ICCAT_database.rds: scripts/01_cleaning/04_convert_iccat_mdb_to_rds.R data/raw/RFMO_data/ICCAT/t2ce_20230131web.mdb
		cd $(<D);Rscript $(<F)

data/processed/rfmo_iccat_tuna_annual_gear_flag.rds: scripts/01_cleaning/05_clean_iccat.R data/raw/RFMO_data/ICCAT/ICCAT_database.rds
		cd $(<D);Rscript $(<F)

data/processed/rfmo_iccat_tuna_monthly_flag.rds: scripts/01_cleaning/05.1_clean_iccat_to_monthly_level.R data/raw/RFMO_data/ICCAT/ICCAT_database.rds
		cd $(<D);Rscript $(<F)

# MPA data --------------------------------------------------------------------
# Clean LMPA data set
data/processed/clean_lmpas.gpkg: scripts/01_cleaning/06_clean_lmpas.R
		cd $(<D);Rscript $(<F)


## Step 2 - MAKE DATA SET ######################################################

# Combine all clean RFMO data into an annual panel
data/processed/rfmo_all_annual_gear_flag.rds: scripts/02_make_data/01_combine_all_rfmos_into_annual.R data/processed/rfmo_iattc_tuna_monthly_gear_flag.rds data/processed/rfmo_wcpfc_tuna_quarterly_gear_flag.rds data/processed/rfmo_iotc_tuna_monthly_gear_flag.rds data/processed/rfmo_iccat_tuna_annual_gear_flag.rds
		cd $(<D);Rscript $(<F)

# Build distance grid
data/processed/distance_grid.rds: scripts/02_make_data/02_build_distance_grid.R data/processed/rfmo_all_annual_gear_flag.rds
		cd $(<D);Rscript $(<F)

# Build annual panel combining RFMO data and distance grid
data/processed/annual_panel.rds: scripts/02_make_data/03_build_annual_panel.R data/processed/distance_grid.rds data/processed/rfmo_all_annual_gear_flag.rds
		cd $(<D);Rscript $(<F)

# Build quarterly panel combining RFMO data and disance grid
data/processed/rfmo_all_qtr_gear_flag.rds: scripts/02_make_data/03.1_build_quarterly_panel.R data/processed/rfmo_iattc_tuna_monthly_gear_flag.rds data/processed/rfmo_iotc_tuna_monthly_gear_flag.rds data/processed/rfmo_iccat_tuna_monthly_flag.rds data/processed/distance_grid.rds
		cd $(<D);Rscript $(<F)

# Build estimation panels. Note two targets in a single rule
data/processed/annual_full_estimation_panel.rds data/processed/annual_relevant_mpa_gears_estimation_panel.rds data/processed/annual_relevant_mpa_gears_and_distances_sensitivity_estimation_panel.rds data/processed/qtr_relevant_mpa_gears_and_distances_sensitivity_estimation_panel.rds: scripts/02_make_data/04_build_estimation_panels.R data/processed/annual_panel.rds
		cd $(<D);Rscript $(<F)

## STEP 3 -  ANALYSIS ##########################################################

# CPUE analysis ----------------------------------------------------------------
# Effect of MPA on CPUE
data/output/main_reg.rds data/output/relevant_mpa_gear_reg.rds: scripts/03_analysis/01_main_regs.R data/processed/annual_full_estimation_panel.rds data/processed/annual_relevant_mpa_gears_estimation_panel.rds
		cd $(<D);Rscript $(<F)

# Effect on CPUE by MPA and species -  note that it's two targets
data/output/gear_mpa_regs.rds data/output/gear_spp_regs.rds: scripts/03_analysis/02_regs_by_mpa_and_spp.R data/processed/annual_relevant_mpa_gears_estimation_panel.rds
		cd $(<D);Rscript $(<F)

## CONTENT #####################################################################

# Figures ----------------------------------------------------------------------

# Figure 1 - Map
results/img/fig1_map.pdf: scripts/04_content/fig1_map.R data/processed/annual_panel.rds
		cd $(<D);Rscript $(<F)

# Figure 2 - Panel of examples of spillover (an dlack thereof)
results/img/fig2_visual_change.pdf: scripts/04_content/fig2_change_in_space_and_time.R data/processed/annual_relevant_mpa_gears_estimation_panel.rds
		cd $(<D);Rscript $(<F)

# Figure 3 - Panel of effect by MPA and species
results/img/fig3_effects_by_mpa_and_spp.pdf: scripts/04_content/fig3_effects_by_mpa_and_spp.R data/output/gear_mpa_regs.rds data/output/gear_spp_regs.rds
		cd $(<D);Rscript $(<F)

# Figure 4 - Alluvial plot showing flow of spillover from MPA-spp-gear-nation

# Fig S1 and S2- Map of overlappping points
results/img/figS1_map_of_overlaps.pdf results/img/figS2_mt_ts_overlaps.pdf: scripts/02_make_data/01_combine_all_rfmos_into_annual.R
		cd $(<D);Rscript $(<F)

# Fig S3 - Map of partially covered grids
results/img/figS3_partially_fully_covered_pts_map.pdf: scripts/04_content/figS3_partially_fully_covered_pts_maps.R data/processed/clean_lmpas.gpkg data/processed/rfmo_all_annual_gear_flag.rds
		cd $(<D);Rscript $(<F)

# Fig S4 and S5 - Comparing DiD vs DiD + FE for mpa and spp
results/img/figS4_effects_by_mpa_with_and_without_fe.pdf results/img/figS5_effects_by_spp_with_and_without_fe.pdf: scripts/04_content/figS4_5_compare_effects_with_and_without_fes.R data/processed/annual_relevant_mpa_gears_estimation_panel.rds data/output/relevant_mpa_gear_combination_model_coefs.rds data/output/gear_mpa_regs.rds data/output/gear_spp_regs.rds

# Tables -----------------------------------------------------------------------
# Table 1 - Main regression table for main text
results/tab/tab1_main_reg_table.tex: tab1_main_reg_table.R data/output/main_reg.rds data/output/relevant_mpa_gear_reg.rds
		cd $(<D);Rscript $(<F)

# Robustness check - Main regressions with difference in means esitmation
results/tab/tabSx_main_regression_table.tex: scripts/05_robustness_checks/01_main_regs_diffrence_in_means.R data/output/main_reg.rds data/output/relevant_mpa_gear_reg.rds
		cd $(<D);Rscript $(<F)

## OTHERS ######################################################################

# Draw makefile dag
workflow.png: Makefile
	make -Bnd | make2graph -b | dot -Tpng -Gdpi=300 -o workflow.png
