###############################################################################
##                       * DATA MERGE & PROCESSING *                         ##
###############################################################################
# -----------------------------------------------------------------------
# Merge in 'aggregate' data

# Population data ----
# Again, the population data has already been reorganised. Therefore, we want to produce population totals and add them to the PRINCIPAL and ENGLAND rows. We should then do a merge but only for the most recent year and rows where the class is England, principal or region in any year

#merge them both separately to avoid problems
combined_data <- combined_data %>%
  left_join(pop_data %>% filter(year == current_year & class != "CLASS TOTAL" | (class %in% c("Region", "Country", "Principal"))), by = c("ons_code", "year")) %>%
  mutate(authority = authority.x,
         class = class.x) %>%
  dplyr:: select(-authority.x, -authority.y, -class.x, -class.y)

combined_data <- combined_data %>%
  left_join(pop_data %>% filter(class == "CLASS TOTAL") %>% dplyr:: select(all_of(population_cols), authority, year), by = c("year", "authority")) %>%
  mutate(population_0to17 = coalesce(population_0to17.x, population_0to17.y),
         population_18to64 = coalesce(population_18to64.x, population_18to64.y),
         population_65plus = coalesce(population_65plus.x, population_65plus.y),
         population_85plus = coalesce(population_85plus.x, population_85plus.y),
         population_total = coalesce(population_total.x, population_total.y),
         population_18plus = coalesce(population_18plus.x, population_18plus.y)) %>%
  dplyr:: select(-population_0to17.x, -population_0to17.y, 
                 -population_18to64.x, -population_18to64.y,
                 -population_65plus.x, -population_65plus.y,
                 -population_85plus.x, -population_85plus.y,
                 -population_total.x, -population_total.y,
                 -population_18plus.x, -population_18plus.y) %>%
  dplyr:: select(year, authority, class, region, ecode, ons_code, everything())

# ASCFR ----
combined_data <- combined_data %>%
  left_join(ASCFR_data, by = "year") %>%  
  mutate(ASC_ASCFR = if_else(class %in% c("ENGLAND", "PRINCIPAL"), ASC_ASCFR, NA_real_))

