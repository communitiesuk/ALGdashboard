###############################################################################
##                                  * CSP *                                  ##
###############################################################################
# Author:     Becky Foster
# Script Description: csp data
# -----------------------------------------------------------------------------

csp_data <- read_excel(CSP_path, sheet = "input_data")

# Years we are interested in for sheet selection
CSP_sheets <- c("csp_2015", "csp_2016", "csp_2017", "csp_2018", "csp_2019", "csp_2020", "csp_2021", "csp_2022", "csp_2023", "csp_2024", "csp_2025")
CSP_years <- c("2015-16", "2016-17", "2017-18", "2018-19", "2019-20", "2020-21", "2021-22", "2022-23", "2023-24", "2024-25", "2025-26")
FYstart <- c("2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023", "2024", "2025")


# Clean the CSP data so we only keep variables relevant to ASC funding and CSP overall. Use the remove_all_but_final_underscore function to clean names so that we can easily pivot longer. Ensure that all variables (bar ID ones) are numeric. Pivot longer into a format we are happy with, and then remove England level summary numbers. We also want to remove the columns ascdelayedreform (constituent part of SCG in 2024), ascgrant (constituent part of SCG in 2022) and asceq (constituent part of SCG in 2022) as they are not consistently reported and are constituent parts of funds. 
csp <- csp_data %>%
  dplyr::select(contains(c('ons', 'authority', 'ecode', 'ibcf', 'winter', 'scsg', 'ascsg', 'scg', 'asc', 'better')), all_of(CSP_sheets)) %>% rename_with(~ sapply(.x, remove_all_but_final_underscore)) %>%
  mutate_at(-c(1:3), as.numeric) %>% 
  pivot_longer(cols=ends_with(c(FYstart)), names_to= c(".value", "year"), names_sep="_") %>%
  filter(ecode!="TE") %>%
  dplyr::select(-c(ascdelayedreform, ascgrant, asceq))
# Put the years into a consistent format 
csp <- csp %>%
  mutate(year = as.numeric(year),
         year = paste0(year, "/", substr(year + 1, 3, 4)))

# we have some duplicate year/ ecode/ ons_code combos, so removing
csp <- csp %>%
  group_by(ecode, year) %>%
  slice_max(csp, n = 1) %>%  # Keep the row with the largest 'csp'
  ungroup()


## Changing reporting level
# Given that CSP is in millions, but we want 1000's for consistency with other datasets (at least initially)

csp <- csp %>%
  mutate(across(
    .cols = where(is.numeric) & !all_of(c("ons_code", "authority", "ecode", "year")),
    .fns = ~ .x * 1000
  ))


colnames(csp)
rm(csp_data)
