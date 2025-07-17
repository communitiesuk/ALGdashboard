#############################################################################
##                                * INPUTS *                                 
##############################################################################

current_year <- "2023/24"
principal_authority_types <- c("SC", "UA", "MD", "LB", "SD")
outturn_years <- c("2015/16", "2016/17", "2017/18", "2018/19", "2019/20", "2020/21", "2021/22", "2022/23", "2023/24")
all_outturn_years <- c("2010/11", "2011/12", "2012/13", "2013/14", "2014/15", "2015/16", "2016/17", "2017/18", "2018/19", "2019/20", "2020/21", "2021/22", "2022/23", "2023/24")

## FILEPATHS ----
pop_data_filepath <- "Inputs/Population/LG_Inform_data - Copy.csv"
revenue_path <- "Q:/ADD Directorate/Local Policy Analysis/LGF/LA data/Revenue/Revenue_R/Final Output/complete_data.xlsx"
CSP_path <- "~/DAP/Spend and funding tool/Spend and Funding Tool/Inputs/CSP/CSP_information_table_LGFS_2025-26.xlsx"
ASCFR_filepath <- "Q:/ADD Directorate/Local Policy Analysis/LGF/LA data/Analysis/Social care Key Stats Pack/Inputs/ASCFR/ASCFR (NCE) Data Tables 2023-24.xlsx"
la_boundaries_filepath <- "Inputs/Geography/CTYUA_DEC_2023_UK_BUC.shp"

## SCRIPT SOURCES ----  
source("data_process/data_population.R")
source("data_process/data_revenue.R")
source("data_process/data_csp.R")
source("data_process/data_ascfr.R")
source("data_process/data_geography.R")
fosteR::get_latest_gdp_deflator(c(current_year, "2015/16"), covid = TRUE)
source("data_process/process_merge.R")
source("data_process/process_aggregated_rows.R")
source("data_process/process_merge_aggregate.R")
source("data_process/process_calculations.R")
source("data_process/process_real_terms.R")
source("data_process/process_index.R")
source("data_process/process_change.R")
source("data_process/final_data_join.R")
