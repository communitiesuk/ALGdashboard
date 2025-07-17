###############################################################################
##                           * REAL TERMS CALCS *                              ##
###############################################################################

print(current_year)
reference_year <- "2015/16"

#Dynamic dates
start_year <- as.numeric(sub("(\\d{4})/\\d{2}", "\\1", current_year))
end_year <- as.numeric(sub("\\d{4}/(\\d{2})", "\\1", current_year)) + 2000
previous_year <- paste0(start_year - 1, "/", substr(end_year - 1, 3, 4))
print(previous_year)

## REAL TERMS CALCS
# For real terms calculations, we don't want to make non-monetary columns deflated.

data_nominal <- combined_data_calcs

finance_columns <- setdiff(names(data_nominal), c(identifying_columns, proportion_variables))

data_real <- data_nominal %>% 
  left_join((gdp_deflator_data %>% dplyr:: select(year, deflator_ratio_2023_24)), by = "year") %>%
  mutate(across(all_of(finance_columns), ~ .x/deflator_ratio_2023_24)) %>% 
  dplyr:: select(-deflator_ratio_2023_24)
  
data_real_2015 <- data_nominal %>% 
  left_join((gdp_deflator_data %>% dplyr:: select(year, deflator_ratio_2015_16)), by = "year") %>%
  mutate(across(all_of(finance_columns), ~ .x/deflator_ratio_2015_16)) %>%
dplyr:: select(-deflator_ratio_2015_16)
