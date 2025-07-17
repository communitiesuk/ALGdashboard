###############################################################################
##                         * POPULATION DATA *                               ##
###############################################################################
# Author:     Becky Foster
# Script Description: Mid-year population estimates inputs
# ----------------------------------------------------------------------------

pop_data <- read_csv(pop_data_filepath, 
                     col_types = cols(`Population aged 65+ (count)` = col_number(), 
                                      `Population aged 0-17` = col_number(), 
                                      `Population aged 18-64` = col_number(), 
                                      `Population aged 85+` = col_number(), 
                                      `Total population` = col_number()))
# Change column names ----
pop_data <- pop_data %>%
  select(-c(5)) %>%
  rename(authority = `area label`,
         class = `area long label`,
         ons_code = area, 
         year = `period label`,
         population_65plus = `Population aged 65+ (count)`,
         population_0to17 = `Population aged 0-17`,
         population_18to64 = `Population aged 18-64`,
         population_85plus = `Population aged 85+`,
         population_total = `Total population`)

# Extract class ----
pop_data <- pop_data %>%
  mutate(class = str_extract(class, "\\(([^)]+)\\)")) %>%  # Extract text inside brackets
  mutate(class = str_replace_all(class, "[()]", ""))  # Remove bracket

# Change year formatting ----
pop_data <- pop_data %>%
  mutate(year = as.numeric(year),
         year = paste0(year, "/", substr(year + 1, 3, 4)))

# Add a principal authority row (principal is all of the dataset anyway, so replicate England row 
pop_data <- pop_data %>%
  bind_rows(
    pop_data %>% 
      filter(authority=="England") %>%
      slice(rep(1:n(), each = n_distinct(pop_data$year))) %>%
      mutate(authority= "Principal", ons_code= "Principal", class = "Principal")
  ) %>%
  distinct(year, authority, .keep_all = TRUE)

# Add all adult population
pop_data <- pop_data %>%
  mutate(population_18plus = population_18to64 + population_65plus)

population_cols <- c("population_65plus", "population_0to17", "population_18to64", "population_85plus", "population_total", "population_18plus")

# Summarise data for each class and year, and combine with original data
pop_data <- pop_data %>%
  bind_rows(
    pop_data %>%
      filter(class == "London borough") %>%
      group_by(year) %>%
      summarise(across(all_of(population_cols), ~ sum(.x, na.rm = TRUE)),
                .groups = "drop") %>%
      mutate(class = "CLASS TOTAL", authority = "London Borough"),
    
    pop_data %>%
      filter(class == "Metropolitan borough") %>%
      group_by(year) %>%
      summarise(across(all_of(population_cols), ~ sum(.x, na.rm = TRUE)),
                .groups = "drop") %>%
      mutate(class = "CLASS TOTAL", authority = "Metropolitan District"),
    
    pop_data %>%
      filter(class == "County") %>%
      group_by(year) %>%
      summarise(across(all_of(population_cols), ~ sum(.x, na.rm = TRUE)),
                .groups = "drop") %>%
      mutate(class = "CLASS TOTAL", authority = "Shire County"),
    
    pop_data %>%
      filter(class == "Unitary") %>%
      group_by(year) %>%
      summarise(across(all_of(population_cols), ~ sum(.x, na.rm = TRUE)),
                .groups = "drop") %>%
      mutate(class = "CLASS TOTAL", authority = "Unitary Authority")
  )
