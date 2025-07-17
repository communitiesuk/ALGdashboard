###############################################################################
##                             * GEOGRAPHY *                                 ##
###############################################################################
# Author:     Becky Foster
# Script Description: lA boundaries/ geography
# -----------------------------------------------------------------------------

la_boundaries <- st_read(la_boundaries_filepath)

la_boundaries <- la_boundaries %>%
  rename(ons_code = CTYUA23CD, authority = CTYUA23NM) %>%
  dplyr:: select(-CTYUA23NMW) %>%
  filter(grepl("^E", ons_code)) %>%
  st_as_sf() %>%
  st_transform(crs = 4326)
