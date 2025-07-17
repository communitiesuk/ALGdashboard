###############################################################################
##                           * DATA JOIN *                              ##
###############################################################################

## ADD LABELS TO EACH DATASET ----
data_nominal$data_type <- "nominal"
data_real$data_type <- "real"
data_real_2015$data_type <- "real_2015"
indexed_data$data_type <- "indexed_2015"
pc_change$data_type <- "pc_change"
pc_change_prev$data_type <- "pc_change_prev"
pp_change$data_type <- "pp_change"
pp_change_prev$data_type <- "pp_change_prev"
indexed_data_2010$data_type <- "indexed_2010"

## RESHAPE EACH DATASET INTO LONG FORMAT ----


prepare_long_data <- function(data, id_cols) {
  data %>%
    pivot_longer(
      cols = -c(any_of(id_cols), data_type),
      names_to = "variable",
      values_to = "value"
    ) %>%
    select(year, authority, ecode, class, variable, value, data_type, everything())
  
}

data_nominal <- prepare_long_data(data_nominal, identifying_columns)
data_real <- prepare_long_data(data_real, identifying_columns)
data_real_2015 <- prepare_long_data(data_real_2015, identifying_columns)
indexed_data <- prepare_long_data(indexed_data, identifying_columns)
indexed_data_2010 <- prepare_long_data(indexed_data_2010, identifying_columns)
pc_change <- prepare_long_data(pc_change, identifying_columns)
pc_change_prev <- prepare_long_data(pc_change_prev, identifying_columns)
pp_change <- prepare_long_data(pp_change, identifying_columns)
pp_change_prev <- prepare_long_data(pp_change_prev, identifying_columns)

## JOIN TOGETHER THE DATASETS ----  
# We should join together using rbind: 
# data_real, data_real_2015, indexed_data, pc_change, pc_change_prev, pp_change, pp_change_prev

data <- bind_rows(
  data_nominal,
  data_real,
  data_real_2015,
  indexed_data,
  indexed_data_2010,
  pc_change,
  pc_change_prev,
  pp_change,
  pp_change_prev
)

## REMOVE NO LONGER NEEDED FILES ----
#rm(aggregated_data, ASCFR_data, combined_data, combined_data_calcs, csp,
  # data_real, data_real_2015, indexed_data, pc_change, pc_change_prev, pp_change, pp_change_prev,
   #reference_values_data, reference_values_data_prev, revenue_data_imputed, pop_data,
   #indexed_data_2010, reference_values_data_2010)

## ADD SCALE VARIABLE ----

# if class == "CLASS TOTAL", scale = class
# if class == "REGION", scale = region
# if class == "ENGLAND", scale = England
# if class == "PRINCIPAL", scale = Principal
# else scale == authority

data <- data %>%
  mutate(
    scale = case_when(
      class == "CLASS TOTAL" ~ "Class",
      class == "REGION" ~ "Region",
      class == "ENGLAND" ~ "England",
      class == "PRINCIPAL" ~ "Principal",
      TRUE ~ "Authority"
    )
  ) %>%
  dplyr:: select(authority, year, ecode, class, variable, value, data_type, scale, everything())


## VARIABLE NAME LABELS ----

variable_labels <- c(
    # ASCFR DATASET
    "ASC_ASCFR" = "ASC NCE (ASC-FR)",
    
    #CALCULATED VARIABLES
    "ASC_CSP_prop" = "ASC as a % of CSP",
    "ASC_GSE_prop" = "ASC as a % of GSE",
    "CSC_CSP_prop" = "CSC as a % of CSP",
    "CSC_GSE_prop" = "CSC as a % of GSE",
    
    # LGF VARIABLES
    "GSE" = "General Service Expenditure (GSE)",
    "RS_NCE_Environment_LGF" = "Environment (LGF definition)",
    "RS_NCE_Other_LGF" = "Other (LGF definition)",
    "RS_NCE_Transport_LGF" = "Transport (LGF definition)",
    "Total_SocialCare" = "Total Social Care",
    "TSE" = "Total Service Expenditure (TSE)",
    
    # CSP DATASET
    "csp" = "Core Spending Power (CSP)",
    "ascdischarge" = "ASC discharge fund",
    "ascreform" = "ASC reform fund",
    "ascsg" = "ASC Support Grant",
    "ascsustainability" = "ASC sustainability fund",
    "bettercare" = "Better Care Fund(BCF)",
    "scg" = "Social Care Grant",
    "scsg" = "Social Care Support Grant",
    "winter" = "Winter grant",
    "ibcf" = "Improved Better Care Fund (iBCF)",
    
    # POPULATION DATA
    "population_65plus" = "65+ population",
    "population_0to17" = "Child (0-17) population",
    "population_18to64" = "Working age (18-64) population",
    "population_85plus" = "85+ population",
    "population_total" = "Total population",
    "population_18plus" = "Adult (18+) population",
    
    # DEFLATOR DATA
    "deflator_ratio_2015_16" = "GDP Deflator Ratio 2015-16",
    "deflator_ratio_2023_24" = "GDP Deflator Ratio 2023-24",
  
    # ASC VARIABLES 
    "NCE_ASC_activ" = "Care assessment and safeguarding",
    "NCE_ASC_activ_18to64" = "Care assessment and safeguarding: 18-64",
    "NCE_ASC_activ_65plus" = "Care assessment and safeguarding: 65+",
    "NCE_ASC_asylum" = "Social support: asylum seekers",
    "NCE_ASC_carer" = "Social support: carers",
    "NCE_ASC_carer_18to64" = "Social support: carers 18-64",
    "NCE_ASC_carer_65plus" = "Social support: carers 65+",
    "NCE_ASC_comm" = "Commissioning, strategy and admin support",
    "NCE_ASC_comm_18to64" = "Commissioning, strategy and admin support: 18-64",
    "NCE_ASC_comm_65plus" = "Commissioning, strategy and admin support: 65+",
    "NCE_ASC_covid" = "Covid",
    "NCE_ASC_cpdisburse" = "Covid disbursements",
    "NCE_ASC_cpother" = "Covid other",
    "NCE_ASC_equip" = "Assisstive equipment and tecnology",
    "NCE_ASC_equip_18to64" = "Assisstive equipment and tecnology: 18-64",
    "NCE_ASC_equip_65plus" = "Assisstive equipment and tecnology: 65+",
    "NCE_ASC_info" = "Information and early intervention",
    "NCE_ASC_info_18to64" = "Information and early intervention: 16-64",
    "NCE_ASC_info_65plus" = "Information and early intervention: 65+",
    "NCE_ASC_isolation" = "Social support: isolation",
    "NCE_ASC_older" = "Older adults",
    "NCE_ASC_olderlearn" = "Learning disability support: 65+",
    "NCE_ASC_oldermem" = "Memory and cognition  support: 65+",
    "NCE_ASC_oldermental" = "Mental health support: 65+",
    "NCE_ASC_olderphys" = "Physical support: 65+",
    "NCE_ASC_oldersens" = "Sensory  support: 65+",
    "NCE_ASC_other" = "Other",
    "NCE_ASC_strat" = "Strategy",
    "NCE_ASC_substance" = "Social support: substance abuse",
    "NCE_ASC_total" = "ASC NCE",
    "NCE_ASC_workinglearn" = "Learning disability support: 18-64",
    "NCE_ASC_workingmem" = "Memory and cognition support: 18-64",
    "NCE_ASC_workingmental" = "lMental health support: 18-64",
    "NCE_ASC_workingphys" = "Physical support: 18-64",
    "NCE_ASC_workingsens" = "Sensory support: 18-64",
    
    # REVENUE OUTTURN NCE'S
    "RS_NCE_AdultSocialCare" = "ASC",
    "RS_NCE_Centralservices" = "Central Services",
    "RS_NCE_ChildrenSocialCare" = "CSC",
    "RS_NCE_ChildrenSocialCare_adj2" = "CSC (adj2)",
    "RS_NCE_Culturalandrelatedserv" = "Cultural",
    "RS_NCE_Educationservices" = "Education",
    "RS_NCE_Educationservices_adj2" = "Education (adj2)",
    "RS_NCE_Eduservices_adj2nosc" = "Education services (adj2 non schools)",
    "RS_NCE_Eduservices_nosc" = "Education services (non schools)",
    "RS_NCE_Environmentalandregulat" = "Environmental and regulation",
    "RS_NCE_Fireandrescueservices" = "Fire & Rescue",
    "RS_NCE_Highwaysandtransportse" = "Transport",
    "RS_NCE_HousingservicesGFRAon" = "Housing",
    "RS_NCE_Otherservices" = "Other",
    "RS_NCE_Planninganddevelopment" = "Planning and development",
    "RS_NCE_Policeservices" = "Police",
    "RS_NCE_PublicHealth" = "Public health",
    "RS_NCE_PublicHealth_nogrant" = "Public health (no grant)",
    "RS_Other_IntegratedTransportAu" = "Other: Integrated Transport",
    "RS_Other_WasteDisposalAuthorit" = "Other: Waste Disposal"
  )
  
## ADD FORMATTED VALUES ----

data <- data %>%
  mutate(
    value_formatted = case_when(
      is.na(value) ~ NA_character_, # Preserve NA values
      
      data_type %in% c("indexed_2010", "indexed_2015") ~
        paste0(ifelse(value - 100 > 0, "+", ""), round(value - 100, 1), "%"),
      
      data_type %in% c("pp_change", "pp_change_prev") ~ 
        paste0(ifelse(value > 0, "+", ""), round(value, 1), "pp"),
      
      data_type %in% c("pc_change", "pc_change_prev") ~ 
        paste0(ifelse(value > 0, "+", ""), round(value, 1), "%"),
      
      data_type %in% c("real", "real_2015", "nominal") & variable %in% proportion_variables ~ 
        paste0(round(value, 1), "%"),
      
      data_type %in% c("real", "real_2015", "nominal") & variable %in% finance_columns ~ 
        paste0("£", comma(value * 1000, accuracy = 1)),
      
      TRUE ~ "CHECK" # Flag anything that doesn't match
    )
  )


save(data, file = "Q:/ADD Directorate/Local Policy Analysis/LGF/LA data/Revenue/Revenue_R/Final Output/dashboard_data.RData")


  # indexed
  # - 100, and if positive, add + and then add % on end
  
  # pp change or pp change previous
  #   + or - and then pp
  # pc change or pc change previous
  #  + or - and %
  
  # real and real 2015
  #   if variable is in proportion variabes, add % on the end
  #   if variable is finance variable, put in terms of £'s (* 1000)

## ADD QUINTILES FOR AUTHORITIES FOR EACH VARIABLE IN FINANCE & PROP