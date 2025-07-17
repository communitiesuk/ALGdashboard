###############################################################################
##                           * CALCULATIONS *                              ##
###############################################################################

# LGF definitions of variables ----
combined_data_calcs <- combined_data  %>%
  mutate(RS_NCE_Other_LGF = rowSums(select(., RS_NCE_Centralservices, RS_NCE_Otherservices), na.rm=TRUE),
         RS_NCE_Environment_LGF = rowSums(select(., RS_NCE_Environmentalandregulat, RS_Other_WasteDisposalAuthorit), na.rm=TRUE),
         RS_NCE_Transport_LGF = rowSums(select(., RS_NCE_Highwaysandtransportse, RS_Other_IntegratedTransportAu), na.rm=TRUE))

# GSE, TSE, Total Social Care ----
combined_data_calcs <- combined_data_calcs %>%
  mutate(GSE = rowSums(select(., RS_NCE_AdultSocialCare, RS_NCE_ChildrenSocialCare_adj2, RS_NCE_Highwaysandtransportse, 
                               RS_NCE_HousingservicesGFRAon, RS_NCE_Culturalandrelatedserv, RS_NCE_Environmentalandregulat,
                               RS_NCE_Planninganddevelopment, RS_NCE_Fireandrescueservices, RS_NCE_Eduservices_adj2nosc, 
                               RS_NCE_Centralservices, RS_NCE_Otherservices, RS_Other_IntegratedTransportAu, 
                               RS_Other_WasteDisposalAuthorit, RS_NCE_PublicHealth), na.rm = TRUE)) %>%  
  mutate(TSE = rowSums(select(., RS_NCE_AdultSocialCare, RS_NCE_ChildrenSocialCare_adj2, RS_NCE_Highwaysandtransportse, 
                              RS_NCE_HousingservicesGFRAon, RS_NCE_Culturalandrelatedserv, RS_NCE_Environmentalandregulat,
                              RS_NCE_Planninganddevelopment, RS_NCE_Fireandrescueservices, RS_NCE_Educationservices_adj2, 
                              RS_NCE_Centralservices, RS_NCE_Otherservices, RS_Other_IntegratedTransportAu, 
                              RS_NCE_Policeservices, RS_NCE_PublicHealth), na.rm = TRUE)) %>%
  mutate(Total_SocialCare = rowSums(select(., RS_NCE_ChildrenSocialCare_adj2, RS_NCE_AdultSocialCare), na.rm=TRUE))


# ASC & CSC----
# Proportions

proportion_variables <- c("ASC_GSE_prop", "ASC_CSP_prop",
                          "CSC_GSE_prop", "CSC_CSP_prop")


combined_data_calcs <- combined_data_calcs %>%
  mutate(ASC_GSE_prop = RS_NCE_AdultSocialCare/GSE,
         ASC_CSP_prop = RS_NCE_AdultSocialCare/csp,
         CSC_GSE_prop = RS_NCE_ChildrenSocialCare_adj2/GSE,
         CSC_CSP_prop = RS_NCE_ChildrenSocialCare_adj2/csp)
