###############################################################################
##                              *SET UP*                                 ##
###############################################################################

FYstart <- c("2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023", "2024", "2025")
CSPyears <- c("csp_2015", "csp_2016", "csp_2017", "csp_2018", "csp_2019", "csp_2020", "csp_2021", "csp_2022", "csp_2023", "csp_2024", "csp_2025")
financial_years <- c("2015/16", "2016/17", "2017/18", "2018/19", "2019/20", "2020/21", "2021/22", "2022/23", "2023/24", "2024/25")
outturn_years <- c("2015/16", "2016/17", "2017/18", "2018/19", "2019/20", "2020/21", "2021/22", "2022/23", "2023/24")
principal_authority_types <- c("SC", "UA", "MD", "LB", "SD")
all_outturn_years <- c("2010/11", "2011/12", "2012/13", "2013/14", "2014/15", "2015/16", "2016/17", "2017/18", "2018/19", "2019/20", "2020/21", "2021/22", "2022/23", "2023/24")


##### load_ct -----
load_ct <- function(ct_path) {
  # List all files in the directory with specified extensions
  file_paths <- list.files(path = ct_path, pattern = "\\.(xlsx|xls|ods)$", full.names = TRUE)
  
  # Loop over each file path
  for (file_path in file_paths) {
    # Determine the file type
    file_type <- tools::file_ext(file_path)
    
    # Extract the base name without extension
    base_name <- tools::file_path_sans_ext(basename(file_path))
    
    # Extract year range from the base name (e.g., "CT 2000" -> "2000")
    year <- str_extract(base_name, "\\d{4}")
    if (!is.na(year)) {
    } else {
      warning(paste("Could not extract year from filename:", file_path))
      next
    }
    
    # Read the sheet names based on file type
    sheets <- if (file_type %in% c("xlsx", "xls")) {
      excel_sheets(file_path)
    } else if (file_type == "ods") {
      ods_sheets(file_path)
    } else {
      warning(paste("Unsupported file type:", file_type))
      next
    }
    
    # Process 'Data' sheet if it exists
    if ("Data" %in% sheets) {
      # Read the sheet with headers at row 4
      data <- if (file_type %in% c("xlsx", "xls")) {
        read_excel(file_path, sheet = "Data", skip = 3)
      } else if (file_type == "ods") {
        read_ods(file_path, sheet = "Data", skip = 3)
      }
      
      # Remove the first row (it's got the years in)
      data <- data[-1, ]
      
      # Dynamically create a variable in the global environment
      assign(paste0("ct_", year), data, envir = .GlobalEnv)
    }
    
    # Process 'Data_Billing' sheet if it exists
    if ("Data_Billing" %in% sheets) {
      # Read the sheet with headers at row 5
      data_billing <- if (file_type %in% c("xlsx", "xls")) {
        read_excel(file_path, sheet = "Data_Billing", skip = 4)
      } else if (file_type == "ods") {
        read_ods(file_path, sheet = "Data_Billing", skip = 4)
      }
      
      # Dynamically create a variable in the global environment
      assign(paste0("ct_", year, "_billing"), data_billing, envir = .GlobalEnv)
    }
    # Process 'Data_Precepting' sheet if it exists
    if ("Data_Precepting" %in% sheets) {
      # Read the sheet with headers at row 5
      data_precepting <- if (file_type %in% c("xlsx", "xls")) {
        read_excel(file_path, sheet = "Data_Precepting", skip = 5)
      } else if (file_type == "ods") {
        read_ods(file_path, sheet = "Data_Precepting", skip = 5)
      }
      
      # Dynamically create a variable in the global environment
      assign(paste0("ct_", year, "_precepting"), data_precepting, envir = .GlobalEnv)
    }
  }
}

##### clean_ct ----
clean_ct <- function(df) {
  # Capture the dataset name
  dataset_name <- deparse(substitute(df))
  
  # Replace `[z]` with `NA`
  replace_na <- function(x) {
    x <- as.character(x)  # Convert to character if not already
    x[x %in%  c("[z]", "-")] <- NA  # Replace `[z]` with `NA`
    return(x)
  }
  # Remove rows where all values are null values
  df_cleaned <- df %>% 
    mutate(across(everything(), replace_na)) %>%  # Replace `[z]` AND '-' with `NA`
    filter(!if_all(everything(), is.na))
  
  # Convert all column names to lowercase and remove hyphens
  names(df_cleaned) <- tolower(names(df_cleaned))
  names(df_cleaned) <- gsub("-", "", names(df_cleaned))
  
  # Filter out rows 
  if ("authority" %in% names(df_cleaned)) {
    df_cleaned <- df_cleaned %>% filter(!is.na(ecode) & !is.na(class) & !(ecode %in% c("Eng", "ILB", "OLB", "MD", "SD", "UA", "MF", "SC", "CFA", "PCC", "CA", "a", "b", "c", "d", "Ecode")) & !grepl("of which", authority, ignore.case = TRUE) & authority != "England" & !is.na(class) & !(class %in% c("GLA", "Class")))
  }
  
  # Find the column(s) that contain the specified text based on dataset name
  if (grepl("ct_\\d{4}$", dataset_name, ignore.case = TRUE)) {
    # Check and rename for "ct_precept_reorg"
    pattern_columns <- grep("total adult social care", names(df_cleaned), ignore.case = TRUE, value = TRUE)
    if (length(pattern_columns) >= 2) {
      last_column <- tail(pattern_columns, 1)
      names(df_cleaned)[names(df_cleaned) == last_column] <- "ct_precept_reorg"
    }
    
    # Check and rename for "ct_precept"
    pattern_column <- grep("10\\. total adult social care precept", names(df_cleaned), ignore.case = TRUE, value = TRUE)
    if (length(pattern_column) > 0) {
      names(df_cleaned)[names(df_cleaned) == pattern_column[1]] <- "ct_precept"
    }
    
  } else if (grepl("ct_\\d{4}_billing", dataset_name, ignore.case = TRUE)) {
    pattern_precept <- grep("10\\. total adult social care precept", names(df_cleaned), ignore.case = TRUE, value = TRUE)
    contains_row_8 <- grepl("row 8", names(df_cleaned), ignore.case = TRUE)
    contains_total_adult_social_care <- grepl("total adult social care", names(df_cleaned), ignore.case = TRUE)
    
    # Logical AND to find columns that satisfy both conditions
    pattern_reorg <- names(df_cleaned)[contains_row_8 & contains_total_adult_social_care]
    if (length(pattern_precept) > 0) {
      names(df_cleaned)[names(df_cleaned) == pattern_precept[1]] <- "ct_precept"
    }
    if (length(pattern_reorg) > 0) {
      names(df_cleaned)[names(df_cleaned) == pattern_reorg[1]] <- "ct_precept_reorg"
    }
  } else if (grepl("ct_\\d{4}_precepting", dataset_name, ignore.case = TRUE)) {
    pattern_column <- grep("4\\. total adult social care precept", names(df_cleaned), ignore.case = TRUE, value = TRUE)
    if (length(pattern_column) > 0) {
      names(df_cleaned)[names(df_cleaned) == pattern_column[1]] <- "ct_precept"
    }
  }
  
  # Define the columns to select
  selected_columns <- c('ecode', 'authority', 'class')
  if ("ct_precept" %in% names(df_cleaned)) {
    selected_columns <- c(selected_columns, 'ct_precept')
  }
  if ("ct_precept_reorg" %in% names(df_cleaned)) {
    selected_columns <- c(selected_columns, 'ct_precept_reorg')
  }
  
  # Select only the desired columns
  df_selected <- df_cleaned %>% select(all_of(selected_columns))
  
  # Convert columns starting with 'ct_' to numeric where possible
  df_final <- df_selected %>%
    mutate(across(starts_with("ct_"), ~ as.numeric(.)))
  
  return(df_final)
}

#### merge_ct ----
merge_ct <- function() {
  # List all objects in the environment
  all_objects <- ls(envir = .GlobalEnv)
  
  # Debugging: Print all object names
  print("All objects in the environment:")
  print(all_objects)
  
  # Identify billing and precepting data frames by pattern
  billing_files <- all_objects[grepl("_billing$", all_objects)]
  precepting_files <- all_objects[grepl("_precepting$", all_objects)]
  
  # Debugging: Print identified files
  print("Billing files:")
  print(billing_files)
  print("Precepting files:")
  print(precepting_files)
  
  # Extract data frames from the environment
  billing_dfs <- lapply(billing_files, get, envir = .GlobalEnv)
  precepting_dfs <- lapply(precepting_files, get, envir = .GlobalEnv)
  
  # Name the lists with the data frame names
  names(billing_dfs) <- billing_files
  names(precepting_dfs) <- precepting_files
  
  # Improved year extraction function
  extract_year <- function(name) {
    # Print the name for debugging
    print(paste("Processing name:", name))
    # Extract the year from names like ct_year_billing or ct_year_precepting
    extracted_year <- str_extract(name, "(?<=ct_)\\d{4}(?=_billing|_precepting)")
    print(paste("Extracted year:", extracted_year))
    return(extracted_year)
  }
  
  # Extract years from the names
  billing_years <- unique(na.omit(sapply(names(billing_dfs), extract_year)))
  precepting_years <- unique(na.omit(sapply(names(precepting_dfs), extract_year)))
  
  # Debugging: Print extracted years
  print("Billing years:")
  print(billing_years)
  print("Precepting years:")
  print(precepting_years)
  
  # Ensure both lists have the same names (years)
  if (!all(billing_years %in% precepting_years) || !all(precepting_years %in% billing_years)) {
    stop("Billing and Precepting data frames must have matching years.")
  }
  
  for (year in billing_years) {
    billing_files_for_year <- names(billing_dfs)[grepl(paste0(year, "_billing$"), names(billing_dfs))]
    precepting_files_for_year <- names(precepting_dfs)[grepl(paste0(year, "_precepting$"), names(precepting_dfs))]
    
    # Debugging: Print matched files for the year
    print(paste("Year:", year))
    print("Billing files for year:")
    print(billing_files_for_year)
    print("Precepting files for year:")
    print(precepting_files_for_year)
    
    if (length(billing_files_for_year) == 1 & length(precepting_files_for_year) == 1) {
      df_billing <- billing_dfs[[billing_files_for_year]]
      df_precepting <- precepting_dfs[[precepting_files_for_year]]
      
      # Row-wise merge
      df_merged <- bind_rows(df_billing, df_precepting)
      
      # Assign the merged data frame to the global environment with the name "ct_year"
      assign(paste0("ct_", year), df_merged, envir = .GlobalEnv)
      
      # Remove the original billing and precepting data frames
      rm(list = c(billing_files_for_year, precepting_files_for_year), envir = .GlobalEnv)
      
    }
  }
}

### combine_ct ----
combine_ct <- function() {
  # Get all dataset names starting with "ct_"
  dataset_names <- ls(envir = .GlobalEnv, pattern = "^ct_\\d{4}$")
  
  # Extract the years from dataset names
  years <- as.integer(sub("^ct_(\\d{4})$", "\\1", dataset_names))
  
  # Ensure there are datasets to process
  if (length(years) == 0) {
    stop("No datasets found matching the pattern 'ct_YYYY'.")
  }
  
  # Initialize an empty list to store the data frames
  datasets <- list()
  
  # Loop through each year and corresponding dataset
  for (year in years) {
    dataset_name <- paste0("ct_", year)
    
    # Check if the dataset exists in the global environment
    if (exists(dataset_name, envir = .GlobalEnv)) {
      df <- get(dataset_name, envir = .GlobalEnv)
      df <- df %>% mutate(year = year)
      datasets[[dataset_name]] <- df
    } else {
      warning(paste("Dataset", dataset_name, "does not exist."))
    }
  }
  
  # Combine all datasets into one
  combined_data <- bind_rows(datasets)
  
  assign("ct", combined_data, envir = .GlobalEnv)
}

####### remove_all_but_final_underscore -----
# This function takes a column name as input. It uses gregexpr to find all underscores in the column name. If there is more than one underscore, it keeps the last underscore and removes all previous underscores.   It is helpful for cleaning CSP file names in order to the pivot to longer format
remove_all_but_final_underscore <- function(col_name) {
  underscores <- gregexpr("_", col_name)[[1]]
  if (length(underscores) > 1) {
    # Extract everything before the last underscore
    last_underscore_pos <- underscores[length(underscores)]
    prefix <- sub("_[^_]*$", "", col_name)  # Remove the last segment
    suffix <- substr(col_name, last_underscore_pos, nchar(col_name))
    paste0(gsub("_", "", prefix), suffix)
  } else {
    col_name
  }
}
