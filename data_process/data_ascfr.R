###############################################################################
##                                * ASC-FR *                                 ##
###############################################################################
# Author:     Becky Foster
# Script Description: England-level asc-fr data
# -----------------------------------------------------------------------------

ASCFR_data <- read_excel(ASCFR_filepath, 
                    sheet = "T4", 
                    skip = 6)
ASCFR_data <- head(ASCFR_data, -4)

ASCFR_data <- ASCFR_data %>% 
  mutate(year = gsub("-", "/", Year)) %>% 
  rename(ASC_ASCFR = 'Cash Terms') %>%
  dplyr:: select(ASC_ASCFR, year)
