library("tidyverse")
library("expss")
source("./data_clean_time.R")
# getwd()
# TODO: in both cases, do not need special handling of col labels
# TODO: export data tables with non timer columns only
export_no_timer <- function(file_path, out_path){
  #Read in the data, skipping the first 3 rows 
  #in some case, the column names imported may not be unique 
  data = read_csv(file_path, skip = 3, col_names = FALSE)
  header_data = read_csv(file_path, skip = 0, n_max = 2, col_names = FALSE)
  # make.unique() to make columns name unique
  variable_names = header_data[1, ] %>% as.character() %>% make.unique()
  variable_labels = header_data[2, ] %>% as.character()
  no_time_labels = variable_labels[!grepl("Timing", 
                                  variable_labels, ignore.case = TRUE)]
  #Assign variable names to the data
  names(data) = variable_names
  data = remove_time_columns(data)
  # rejoin data 
  # remove all time related columns, by column names no Timer/Time
  # all time related columns have a label starts with Timing 
  data = rbind(no_time_labels, data)
  write_csv(data, out_path)
}

# TODO: export data tables with only character columns 

export_character_only <- function(file_path, out_path){
  message("Please only use this function for unique column names and no timer data!")
  variable_names = read_csv(file_path, skip = 0, 
                            n_max = 1, col_names = FALSE) %>% 
                 as.character() %>% make.unique()
  data = read_csv(file_path, skip = 2, col_names = FALSE) 
  names(data) = variable_names
  write_csv(data %>% select(where(is.character)), out_path)
} 

# TODO: test
export_no_timer("../data/raw_data/Pilot Risk soft launch data_numeric values.csv",
                "../data/processing/no_time_pilot_risk_soft.csv")

export_character_only("../data/processing/no_time_pilot_risk_soft.csv",
                      "../data/processing/text_only_pilot_risk_soft.csv")