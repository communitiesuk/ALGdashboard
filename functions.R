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
