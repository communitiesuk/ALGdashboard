###############################################################################
##                           * INDEXED DATA *                              ##
###############################################################################

# first we need one row per authority-year combo ----

aggregated_data <- data_real %>%
  group_by(ecode, year) %>%
  summarise(
    across(all_of(c(proportion_variables, finance_columns)), 
           ~ if (all(is.na(.))) NA_real_ else max(., na.rm = TRUE)),
    across(any_of(setdiff(identifying_columns, c("ecode", "year"))), ~ first(.)),
    .groups = "drop"
  )

# extract 2015 reference values ----
reference_values_data <- aggregated_data %>%
  filter(year == reference_year) %>%
  select(ecode, all_of(c(finance_columns, proportion_variables))) %>%
  rename_with(~ paste0(.x, "_ref"), all_of(c(finance_columns, proportion_variables)))  # Rename reference columns

# perform indexing ----
indexed_data <- aggregated_data %>%
  left_join(reference_values_data, by = "ecode") %>%  # Join reference values
  mutate(across(all_of(finance_columns), 
                ~ ifelse(!is.na(get(paste0(cur_column(), "_ref"))), 
                         . / get(paste0(cur_column(), "_ref")) * 100, 
                         NA_real_))) %>%  # Perform indexing
  select(-ends_with("_ref"))  # Remove reference columns

# extract 2010 reference values ----
reference_values_data_2010 <- aggregated_data %>%
  filter(year == "2010/11") %>%
  select(ecode, all_of(c(finance_columns, proportion_variables))) %>%
  rename_with(~ paste0(.x, "_ref"), all_of(c(finance_columns, proportion_variables))) 

# perform indexing ----
indexed_data_2010 <- aggregated_data %>%
  left_join(reference_values_data_2010, by = "ecode") %>%  # Join reference values
  mutate(across(all_of(finance_columns), 
                ~ ifelse(!is.na(get(paste0(cur_column(), "_ref"))), 
                         . / get(paste0(cur_column(), "_ref")) * 100, 
                         NA_real_))) %>%  # Perform indexing
  select(-ends_with("_ref"))  # Remove reference columns

