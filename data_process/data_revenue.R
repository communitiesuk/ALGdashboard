###############################################################################
##                             * REVENUE *                                   ##
###############################################################################
# Author:     Becky Foster
# Script Description: Revenue Outturn inputs
# -----------------------------------------------------------------------------

# Load in data (this can take some time!)
revenue_data_original <- read_excel(revenue_path, sheet = "Sheet 1")

# Check for missing authorities by year
table(revenue_data_original$missing_outturn, revenue_data_original$year)

# Remove summary rows (we will add these back in later)
revenue_data_original <- revenue_data_original %>%
  filter(!ecode =="Eng") 

# Check that the missing authorities match the numbers in the RO technical release
revenue_data_original %>% filter(missing_outturn==1) %>% reframe(authority, year)

# Label the identifying columns in the database
identifying_revenue <- c("ecode", "ons_code", "year", "authority", "class", "region", "missing_outturn", "missing_budget", "long_class", "rural", "rurality", "coastal", "Combined_Authorities", "rcode", "ecode_2", "current_auth_ecode", "clg_code", "current", "start_year", "end_year", "billing_authority", "old_code", "rsgs", "lacode", "laser", "doe", "dh", "ceremonial_county", "exp", "DfEcode")         

## IMPUTED REVENUE DATABASE ----
## Define the variables we want to impute for ----
lgf_vars <- c("RS_NCE_ChildrenSocialCare_adj2",   
              "RS_NCE_AdultSocialCare",
              "RS_NCE_Educationservices",
              "RS_NCE_ChildrenSocialCare", 
              "RS_NCE_PublicHealth", 
              "RS_NCE_Highwaysandtransportse", 
              "RS_NCE_HousingservicesGFRAon",
              "RS_NCE_Culturalandrelatedserv",
              "RS_NCE_Environmentalandregulat",
              "RS_NCE_Planninganddevelopment",
              "RS_NCE_Policeservices",
              "RS_NCE_Fireandrescueservices",
              "RS_NCE_Centralservices",
              "RS_NCE_Otherservices",
              "RS_NCE_Eduservices_nosc",
              "RS_NCE_PublicHealth_nogrant",
              "RS_NCE_Eduservices_adj2nosc",
              "RS_Other_IntegratedTransportAu",
              "RS_Other_WasteDisposalAuthorit",
                "RS_SUM_inc",
                "RS_Other_LocalServicesSupport",
                "RS_Other_insideAEFgrant",
                "RS_Other_Interauthoritytransfe",
                "RS_Other_PoliceGrant",
                "RS_Other_RetainedincomefromRa",
                "RS_Other_BusinessRatesSuppleme",
                "RS_Other_RevenueSupportGrant",
                "RS_Other_RevenueFinancingOthe",
                "RS_Other_ParishPrecepts",
                "RS_Other_COUNCILTAXREQUIREMENT",
                "RG_inAEF_edu",
                "RG_inAEF_pubhealth",
                "RS_SUM_otherinc",
                "RS_Other_Capitalchargesext",
                "RS_Other_ExternalTradingAccoun",
                "RS_Other_Capitalrecflex",
                "RS_Other_Interestandinvestment",
                "RS_Other_CommunityInfrastructur",
                "RS_Other_Capitalchargesint",
                "RS_Other_InternalTradingAccoun",
                "RG_outAEF_rentallow",
                "RG_outAEF_rentrebateoutHRA",
                "RG_outAEF_rentrebateHRA",
                "RG_outAEF_edu",
                "RS_SUM_inc_adj",
                "RS_NCE_TOTALSERVICEEXPENDITURE",
                "RS_Other_Housingbenefitsrent",
                "RS_Other_HousingbenefitsnonH",
                "RS_Other_housingbenrentre",
                "RS_Other_Housingbenefitssubsi",
                "RS_Other_ContributiontotheHRA",
                "RS_SUM_nonservicespend_debt",
                "RS_SUM_nonservicespend",
                "RS_SUM_servicespend",
                "RS_SUM_resinc",
                "RS_Other_Appropfinadj",
                "RS_Other_Appropunequalpay",
                "RS_Other_Appropabsences",
                "RS_Other_Adjustmentstonetcurr",
                "RS_Other_LondonPensionsFundAu",
                "RS_Other_Otherlevies",
                "RS_Other_Capitalexpenditurecha",
                "RS_Other_CapitalexpenditurePH",
                "RS_Other_CarbonReductionexp",
                "RS_Other_CarbonReductioninc",
                "RS_Other_Interestpayableandsi",
                "RS_Other_HRAItem8Interestpa",
                "RS_Other_LeasingPaymentsmetfr",
                "RS_Other_LevyEnvironmentAgenc",
                "RS_Other_Provisionforbaddebts",
                "RS_Other_ProvisionforRepayment",
                "RS_Other_outsideAEFgrant",
                "RS_Other_other",
                "RS_SUM_unringres31Mar_adj",
                "RS_Other_Unallocatedres31Mar",
                "RS_Other_Otherearmarkedres31Mar",
                "RG_inAEF_total",
                "RG_outAEF_total",
                "RG_inAEF_HomelessnessWTU",
                "RG_inAEF_HBadmin",
                "RG_inAEF_PFI",
                "RG_inAEF_RSInitiative",
                "RG_inAEF_other",
                "RG_outAEF_other",
                "RS_Other_COVID_add_bsns_res",
              "RS_NCE_Educationservices_adj2",
              "RS_Other_NNDRsd",
              "RS_Other_GeneralGLAGrant",
              "RS_Other_AreaBasedGrantABG",
              "RS_Other_CovidunringFunding",
              "RS_Other_Salesfeesandchargescomp",
              "RS_Other_Tax_Income_Guarantee",
              "RS_Other_LCTS_grant",
              "RS_Other_Ratesbroughtforward_2",
              "RS_Other_expanded_brr_relief",
              "RS_Other_RedistributedNonDomes",
              "RG_inAEF_GLAtrans",
              "RG_inAEF_covidwintersupport",
              "RG_inAEF_COMFund",
              "RG_inAEF_WorkforceRRfund",
              "RG_inAEF_ASomicron",
              "RG_inAEF_WBFund"
              )
                
                
                
ASC_vars <- grep("^NCE_ASC", names(revenue_data_original), value = TRUE)

variables_to_impute <- c(lgf_vars, ASC_vars)

fire_variables_exclusive <- names(revenue_data_original) %>% 
  grep("fire", ., ignore.case = TRUE, value = TRUE)

# we want to force these to be 0
grant_variables <- names(revenue_data_original) %>%
  grep("^RG_", ., value = TRUE)

## Produce growth ratios and sense check them ----
growth_ratios_result <- fosteR:: calculate_growth_ratios(revenue_data_original, variables_to_impute)
growth_ratios <- growth_ratios_result$growth_ratios
old_variables <- growth_ratios_result$old_variables
new_variables <- growth_ratios_result$new_variables

## Impute for missing values ----
revenue_data_imputed <- fosteR:: impute_missing_values(revenue_data_original, variables_to_impute, growth_ratios, old_variables, new_variables, fire_variables_exclusive, grant_variables)

## Return a dataset that only has identifying columns and the variables we have imputed for ----
#(helps to avoid accidentally using a variable we haven't imputed for)
revenue_data_imputed <- revenue_data_imputed %>% dplyr:: select(year, authority, ecode, class, region, missing_outturn, ons_code, all_of(variables_to_impute)) %>% filter(year %in% all_outturn_years)


## Correct some incorrect ONS_codes
# North Yorkshire & Somerset
revenue_data_imputed <- revenue_data_imputed %>%
  mutate(ons_code = case_when(
    year >= "2023/24" & authority == "Somerset" ~ "E06000066",
    year >= "2023/24" & authority == "North Yorkshire" ~ "E06000065",
    TRUE ~ ons_code
  ))

# Remove no longer required environment objects
rm(growth_ratios, growth_ratios_result, new_variables, old_variables, revenue_data_original)
