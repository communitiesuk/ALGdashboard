###############################################################################
##                       * DATA MERGE & PROCESSING *                         ##
###############################################################################
# -----------------------------------------------------------------------
# Produce England, Principal, Regional and Class-level summary rows


identifying_columns <- c(population_cols, identifying_revenue)
finance_columns <- setdiff(names(combined_data), c(identifying_columns))

## PRINCIPAL AND ENGLAND ----
combined_data <- combined_data %>%
  bind_rows(
    combined_data %>%
      group_by(year) %>%
      summarise(across(all_of(finance_columns), \(x) sum(x, na.rm = TRUE)),
                .groups = "drop") %>%
      mutate(class = "ENGLAND", authority = "England", ecode = "ENGLAND", ons_code = "E92000001"),
    
    combined_data %>%
      filter(class %in% principal_authority_types) %>%
      group_by(year) %>%
      summarise(across(all_of(finance_columns), \(x) sum(x, na.rm = TRUE)),
                .groups = "drop") %>%
      mutate(class = "PRINCIPAL", authority = "Principal", ecode = "PRINCIPAL", ons_code = "Principal"),
  )

## REGIONAL LEVEL ----
combined_data <- combined_data %>%
  bind_rows(
    combined_data %>%
      filter(region == "EM") %>%
      group_by(year) %>%
      summarise(across(all_of(finance_columns), \(x) sum(x, na.rm = TRUE)),
                .groups = "drop") %>%
      mutate(class="REGION", authority = "East Midlands", ons_code = "E12000004", ecode="EM"),
    
    combined_data %>%
      filter(region == "EE") %>%
      group_by(year) %>%
      summarise(across(all_of(finance_columns), \(x) sum(x, na.rm = TRUE)),
                .groups = "drop") %>%
      mutate(class="REGION", authority = "East of England", ons_code = "E12000006", ecode="EE"),
    
    combined_data %>%
      filter(region == "L") %>%
      group_by(year) %>%
      summarise(across(all_of(finance_columns), \(x) sum(x, na.rm = TRUE)),
                .groups = "drop") %>%
      mutate(class="REGION", authority = "London", ons_code = "E12000007"),
    
    combined_data %>%
      filter(region == "NE") %>%
      group_by(year) %>%
      summarise(across(all_of(finance_columns), \(x) sum(x, na.rm = TRUE)),
                .groups = "drop") %>%
      mutate(class="REGION", authority = "North East", ons_code = "E12000001", ecode = "NE"),
    
    combined_data %>%
      filter(region == "NW") %>%
      group_by(year) %>%
      summarise(across(all_of(finance_columns), \(x) sum(x, na.rm = TRUE)),
                .groups = "drop") %>%
      mutate(class="REGION", authority = "North West", ons_code = "E12000002"),
    
    combined_data %>%
      filter(region == "SE") %>%
      group_by(year) %>%
      summarise(across(all_of(finance_columns), \(x) sum(x, na.rm = TRUE)),
                .groups = "drop") %>%
      mutate(class="REGION", authority = "South East", ons_code = "E12000008", ecode="SE"),
    
    combined_data %>%
      filter(region == "SW") %>%
      group_by(year) %>%
      summarise(across(all_of(finance_columns), \(x) sum(x, na.rm = TRUE)),
                .groups = "drop") %>%
      mutate(class="REGION", authority = "South West", ons_code = "E12000009", ecode="SW"),
    
    combined_data %>%
      filter(region == "WM") %>%
      group_by(year) %>%
      summarise(across(all_of(finance_columns), \(x) sum(x, na.rm = TRUE)),
                .groups = "drop") %>%
      mutate(class="REGION", authority = "West Midlands", ons_code = "E12000005", ecode="WM"),
    
    combined_data %>%
      filter(region == "YH") %>%
      group_by(year) %>%
      summarise(across(all_of(finance_columns), \(x) sum(x, na.rm = TRUE)),
                .groups = "drop") %>%
      mutate(class="REGION", authority = "Yorkshire and Humberside", ons_code = "E12000003", ecode="YH")
  ) %>%
  dplyr:: select(year, authority, ecode, ons_code, class, region, everything())

## CLASS-LEVEL ----
combined_data <- combined_data %>%
  bind_rows(
    combined_data %>%
      filter(class == "CA") %>%
      group_by(year) %>%
      summarise(across(all_of(finance_columns), \(x) sum(x, na.rm = TRUE)),
                .groups = "drop") %>%
      mutate(class="CLASS TOTAL", authority = "Combined Authority"),
    
    combined_data %>%
      filter(class == "FIR") %>%
      group_by(year) %>%
      summarise(across(all_of(finance_columns), \(x) sum(x, na.rm = TRUE)),
                .groups = "drop") %>%
      mutate(class="CLASS TOTAL", authority = "Fire Authority"),
    
    combined_data %>%
      filter(class == "GLA") %>%
      group_by(year) %>%
      summarise(across(all_of(finance_columns), \(x) sum(x, na.rm = TRUE)),
                .groups = "drop") %>%
      mutate(class="CLASS TOTAL", authority = "Greater London Authority", ),
    
    combined_data %>%
      filter(class == "LB") %>%
      group_by(year) %>%
      summarise(across(all_of(finance_columns), \(x) sum(x, na.rm = TRUE)),
                .groups = "drop") %>%
      mutate(class="CLASS TOTAL", authority = "London Borough"),
    
    combined_data %>%
      filter(class == "MD") %>%
      group_by(year) %>%
      summarise(across(all_of(finance_columns), \(x) sum(x, na.rm = TRUE)),
                .groups = "drop") %>%
      mutate(class="CLASS TOTAL", authority = "Metropolitan District"),
    
    combined_data %>%
      filter(class == "PARK") %>%
      group_by(year) %>%
      summarise(across(all_of(finance_columns), \(x) sum(x, na.rm = TRUE)),
                .groups = "drop") %>%
      mutate(class="CLASS TOTAL", authority = "Park Authority"),
    
    combined_data %>%
      filter(class == "POL") %>%
      group_by(year) %>%
      summarise(across(all_of(finance_columns), \(x) sum(x, na.rm = TRUE)),
                .groups = "drop") %>%
      mutate(class="CLASS TOTAL", authority = "Police Authority"),
    
    combined_data %>%
      filter(class == "SC") %>%
      group_by(year) %>%
      summarise(across(all_of(finance_columns), \(x) sum(x, na.rm = TRUE)),
                .groups = "drop") %>%
      mutate(class="CLASS TOTAL", authority = "Shire County"),
    
    combined_data %>%
      filter(class == "SD") %>%
      group_by(year) %>%
      summarise(across(all_of(finance_columns), \(x) sum(x, na.rm = TRUE)),
                .groups = "drop") %>%
      mutate(class="CLASS TOTAL", authority = "Shire District"),
    
    combined_data %>%
      filter(class == "TRANS") %>%
      group_by(year) %>%
      summarise(across(all_of(finance_columns), \(x) sum(x, na.rm = TRUE)),
                .groups = "drop") %>%
      mutate(class="CLASS TOTAL", authority = "Transport Authority"),
    
    combined_data %>%
      filter(class == "UA") %>%
      group_by(year) %>%
      summarise(across(all_of(finance_columns), \(x) sum(x, na.rm = TRUE)),
                .groups = "drop") %>%
      mutate(class="CLASS TOTAL", authority = "Unitary Authority")
  ) %>%
  dplyr:: select(year, authority, ecode, ons_code, class, region, everything())



