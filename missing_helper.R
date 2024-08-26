library(tidyverse)

# generate missing flag corresponding to a certain column 
generate_missingness_flag = function(data, column_name) {
  # Create the dynamic column name
  new_column_name = paste0("missingness_", column_name)
  
  # Create the categorical column using mutate and ifelse
  data = data %>%
    # using the bang bang operator to dynamically evaluate
    
    mutate(!!new_column_name := ifelse(is.na(.data[[column_name]]), 
                                      "missing", "nonmissing"))
  
  # Return the updated data frame
  return(data)
}


# Function to check and summarize missingness by specified columns
summarize_missingness = function(data, columns_to_check) {
  # Ensure the specified columns are present in the data
  columns_to_check = intersect(columns_to_check, names(data))
  
  # Calculate missingness for the specified columns
  missing_summary = data %>%
    select(all_of(columns_to_check)) %>%  # Select only the specified columns
    summarise(across(everything(), ~ sum(is.na(.)))) %>%  # Count missing values by column
    pivot_longer(cols = everything(), 
                 names_to = "Column", values_to = "Missing_Count") %>%
    mutate(Missingness_percentage = paste0(round((Missing_Count / nrow(data)) * 100, 2), 
                                           "%")) %>%
    arrange(desc(Missing_Count))  # Arrange by highest missing values
  
  # Return the summary table
  return(missing_summary)
}

