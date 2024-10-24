export_with_labels <- function(dataframe, filepath = NULL) {
  
  # Get the name of the dataframe as a string
  dataframe_name <- deparse(substitute(dataframe))
  
  # If filepath is not provided, create a default one
  if (is.null(filepath)) {
    filepath <- paste0("output/", dataframe_name, ".xlsx")
  }
  
  # Extract labels for each column, providing a default "" if a label is missing
  labels <- sapply(dataframe, function(x) ifelse(is.null(attr(x, "label")), "", attr(x, "label")))
  column_names <- names(dataframe)
  
  # Create data frames for labels and column names
  label_row <- data.frame(matrix(labels, nrow = 1, dimnames = list(NULL, column_names)), stringsAsFactors = FALSE)
  col_name_row <- data.frame(matrix(column_names, nrow = 1, dimnames = list(NULL, column_names)), stringsAsFactors = FALSE)
  
  # Convert the original dataframe to characters to ensure consistency
  data_df <- as.data.frame(lapply(dataframe, as.character), stringsAsFactors = FALSE)
  
  # Combine the label row, column name row, and data into a single dataframe
  export_df <- bind_rows(label_row, col_name_row, data_df)
  
  # Write the dataframe to an Excel file without including column names
  write_xlsx(export_df, filepath, col_names = FALSE)
  
  # Return the filepath as a confirmation
  return(filepath)
}


# Vectorized function to convert CMC to "yyyy-mm"
cmc_to_yyyymm <- function(cmc) {
  
  # Convert CMC to "yyyy-mm" format, handling NA values
  ifelse(
    is.na(cmc),
    NA_character_,  # Return NA for missing values
    sprintf(
      "%04d-%02d",
      (cmc - 1) %/% 12 + 1900,  # Calculate year from CMC
      (cmc - 1) %% 12 + 1       # Calculate month from CMC
    )
  )
}

