###############################################################################
##                       * DATA MERGE & PROCESSING *                         ##
###############################################################################
# -----------------------------------------------------------------------
## Merge required datasets to create a nominal terms dataset ----
# CSP ----
combined_data <- revenue_data_imputed %>% 
  full_join(csp, by=c("year", "ecode")) %>%
  mutate(authority = coalesce(authority.x, authority.y),
         ons_code = coalesce(ons_code.x, ons_code.y)) %>%
  dplyr:: select(-authority.x, -authority.y, -ons_code.x, -ons_code.y) %>%
  dplyr:: select(year, authority, ecode, ons_code, class, region, csp, everything())
