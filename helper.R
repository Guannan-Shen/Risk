# Function to convert Excel-style column reference (like "QT") to a numeric index
excel_column_to_number = function(column_ref) {
  column_ref = toupper(column_ref)  # Ensure the reference is in uppercase
  length = nchar(column_ref)
  column_number = 0
  
  for (i in 1:length) {
    # Convert each character to its numeric equivalent: A=1, B=2, ..., Z=26
    column_number = column_number * 26 + (utf8ToInt(substr(column_ref, i, i)) 
                                          - utf8ToInt("A") + 1)
  }
  # colnames(data)[462], the 462th column is column QT in Microsoft Excel QT
  return(column_number)
}


# Define the function to create the Pass_screening column and label its levels
create_screen_label = function(data, screening_cols) {
  # Create the pass_screening column
  data_with_screening = data %>%
    mutate(Pass_screening = 
             if_else(rowSums(across(all_of(screening_cols)) == 1) > 0, 
                     0, 1)) %>%
    # Convert pass_screening to a factor and label levels using forcats
    mutate(Pass_screening = Pass_screening %>%
             as_factor() %>%
             fct_recode("Yes" = "1", "No" = "0"))
  
  # Return the dataset with the pass_screening column added and labeled
  return(data_with_screening)
}


# Function to summarize descriptive statistics for selected columns with optional renaming by labels
summarize_statistics = function(data, columns_to_summarize, rename_by_label = TRUE) {
  # Ensure the specified columns are present in the data
  columns_to_summarize = intersect(columns_to_summarize, names(data))
  
  # Get variable labels if rename_by_label is TRUE
  if (rename_by_label) {
    labels = sapply(data[columns_to_summarize], var_lab)
  } else {
    labels = NULL
  }
  
  # Select the specified columns first
  data_selected = data %>% select(all_of(columns_to_summarize))
  
  # Optionally rename columns by variable labels before summary
  if (rename_by_label && !is.null(labels)) {
    data_selected = data_selected %>%
      rename_with(.fn = ~ labels[.x], .cols = everything())
  }
  
  # Calculate descriptive statistics
  summary_stats = data_selected %>%
    summarise(across(
      everything(), 
      list(
        mean = ~ mean(. , na.rm = TRUE),
        median = ~ median(. , na.rm = TRUE),
        min = ~ min(. , na.rm = TRUE),
        max = ~ max(. , na.rm = TRUE),
        variance = ~ var(. , na.rm = TRUE),
        sd = ~ sd(. , na.rm = TRUE),
        n_missing = ~ sum(is.na(.))
      ),
      .names = "{col}_{fn}"
    ))
  
  # Return the summarized statistics
  return(summary_stats)
}


# Function to rename specified columns by their labels
rename_columns_by_labels = function(data, columns_to_rename = NULL) {
  # If no columns are specified, rename all columns
  if (is.null(columns_to_rename)) {
    columns_to_rename = names(data)
  }
  # Filter out columns that are not in the data
  columns_to_rename = intersect(columns_to_rename, names(data))
  
  # Get the labels for the specified columns
  labels = sapply(data[columns_to_rename], var_lab)
  
  # Replace empty labels with original column names
  labels = ifelse(labels == "", columns_to_rename, labels)
  
  # Rename the specified columns using the labels
  names(data)[names(data) %in% columns_to_rename] = labels
  
  return(data)
}


