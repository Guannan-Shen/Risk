library("tidyverse")
library("expss")
# getwd()
load_var_label <- function(file_path) {

  #Read in the data, skipping the first 3 rows 
  data = read_csv(file_path, skip = 3, col_names = FALSE)
  
  #Read in the first two rows separately for variable names and labels
  header_data = read_csv(file_path, skip = 0, n_max = 2, col_names = FALSE)
  
  #Extract variable names and labels
  # make.unique() to make columns name unique
  variable_names = header_data[1, ] %>% as.character() %>% make.unique()
  variable_labels = header_data[2, ] %>% as.character()
  
  #Assign variable names to the data
  names(data) = variable_names
  
  # create list structure for labels 
  labels_list = list()
  for (i in seq_along(variable_names)) {
    labels_list[[variable_names[i]]] = variable_labels[i]
  }
  
  # Step 5: Apply the variable labels using the expss package
  data = apply_labels(data, labels_list)
  
  # Return the cleaned and labeled dataset
  return(data)
}

# Example usage:
file_path = "../data/raw_data/Pilot Risk soft launch data_numeric values.csv"  
cleaned_data = load_var_label(file_path)
saveRDS(cleaned_data, "../data/processing/data_labels.rds")
# Inspect the cleaned data
# view(cleaned_data)
# sapply(cleaned_data, var_lab)
