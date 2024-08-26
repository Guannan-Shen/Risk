# # at this early stage of proj, using this to install and load package
# # later on, load specific pkgs individually.
# source("./pkg_maintain.R")
library(tidyverse)

# remove all time related columns, by column names 
# all time related columns have a label starts with Timing 
# remove all columns containing time
remove_time_columns = function(data){
  # Identify timing-related columns (columns containing "Timer" or "Time")
  # ignore case , ignore.case = TRUE
  time_columns <- grep("Timer|Time", names(data), value = TRUE)
  data <- data %>%
    select(-one_of(time_columns))
  return(data)
}

# draft code, untested
# Function to handle time-related columns
handle_time_columns <- function(data,
                                action = c("exclude", "summarize", "analyze")) {
  # Choose an action: "exclude" (default), "summarize", or "analyze"
  action <- match.arg(action)
  
  # Identify timing-related columns (columns containing "Timer" or "Time")
  time_columns <- grep("Timer|Time", names(data), value = TRUE)
  
  if (action == "exclude") {
    # Exclude time-related columns
    data <- data %>%
      select(-one_of(time_columns))
    return(data)
    
  } else if (action == "summarize") {
    # Summarize time-related columns (e.g., mean, median)
    summary_stats <- data %>%
      select(one_of(time_columns)) %>%
      summarise_all(list(
        mean = ~ mean(. , na.rm = TRUE),
        median = ~ median(. , na.rm = TRUE)
      ))
    return(summary_stats)
    
  } else if (action == "analyze") {
    # Perform analysis on time-related columns (e.g., histogram for each time variable)
    par(mfrow = c(2, 2))  # Set plot layout if you have many time variables
    data %>%
      select(one_of(time_columns)) %>%
      purrr::map( ~ hist(
        .,
        main = paste("Histogram of", names(.)),
        xlab = "Time (seconds)",
        col = "skyblue"
      ))
    par(mfrow = c(1, 1))  # Reset plot layout
  }
}
