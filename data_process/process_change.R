###############################################################################
##                           * INDEXED DATA *                              ##
###############################################################################

#### PERCENTAGE CHANGE & PERCENTAGE POINT CHANGE SINCE REFERENCE YEAR ####
pc_change <- aggregated_data %>%
  left_join(reference_values_data, by = "ecode") %>%
  mutate(across(all_of(c(finance_columns, proportion_variables)), 
                ~ ifelse(!is.na(get(paste0(cur_column(), "_ref"))), 
                         ((. - get(paste0(cur_column(), "_ref"))) / get(paste0(cur_column(), "_ref"))) * 100, 
                         NA_real_), 
                .names = "{col}")) %>%
  select(-ends_with("_ref"))

pp_change <- aggregated_data %>%
  left_join(reference_values_data, by = "ecode") %>%
  mutate(across(all_of(proportion_variables), 
                ~ ifelse(!is.na(get(paste0(cur_column(), "_ref"))), 
                         (. - get(paste0(cur_column(), "_ref"))) * 100, 
                         NA_real_), 
                .names = "{col}"))  %>%
  select(-ends_with("_ref")) %>%
  dplyr:: select(any_of(identifying_columns), any_of(proportion_variables))


#### PERCENTAGE CHANGE & PERCENTAGE POINT CHANGE SINCE MOST RECENT YEAR ####
reference_values_data_prev <- aggregated_data %>%
  filter(year == previous_year) %>%
  select(ecode, all_of(c(finance_columns, proportion_variables))) %>%
  rename_with(~ paste0(.x, "_ref"), all_of(c(finance_columns, proportion_variables)))  # Rename reference columns

pc_change_prev <- aggregated_data %>%
  filter(year >= previous_year) %>%
  left_join(reference_values_data_prev, by = "ecode") %>%
  mutate(across(all_of(c(finance_columns, proportion_variables)), 
                ~ ifelse(!is.na(get(paste0(cur_column(), "_ref"))), 
                         ((. - get(paste0(cur_column(), "_ref"))) / get(paste0(cur_column(), "_ref"))) * 100, 
                         NA_real_), 
                .names = "{col}")) %>%
  select(-ends_with("_ref")) %>%
  dplyr:: select(any_of(identifying_columns), everything())

pp_change_prev <- aggregated_data %>%
filter(year >= previous_year) %>%
  left_join(reference_values_data_prev, by = "ecode") %>%
  mutate(across(all_of(proportion_variables), 
                ~ ifelse(!is.na(get(paste0(cur_column(), "_ref"))), 
                         (. - get(paste0(cur_column(), "_ref"))) * 100, 
                         NA_real_), 
                .names = "{col}")) %>%
  select(-ends_with("_ref")) %>%
  dplyr:: select(any_of(identifying_columns), any_of(proportion_variables))


