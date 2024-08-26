library(tidyverse)
library(expss)
source("./helper.R")

data <- readRDS("../data/processing/data_labels.rds")
# colnames(data)
# sapply(cleaned_data, var_lab)

# check list 1, columns to screen out subjects
#  generate column pass_screening
#  output tables in rmarkdown, PDF
screening_cols <- c("PriorBreastCancer", "BreastRemoved", "ChestRad",
                    "DCIS-LCIS", "BRCA1/2", "CowdenSyn", "LiFraumeni")
data <- data %>% create_screen_label(screening_cols)

# TODO: filter data by rows, then check missingness for important columns
data$Pass_screening

# check list 2: check missing, especially for the following columns
# finish risk list 
complete_cols <- c("Experimental condition", "abs_risk_rounded")
data %>% select(all_of(complete_cols))


# 5.	Check for minimal missingness on risk rejection questions 
# Q1:RReject1 Q2:RReject2 (Note, this is an open text, free response variable)
# Q3:RReject3 Q4:RReject4 Q5:RReject5 Q6:RReject6 Q7:RReject7

risk_rej_cols <- c("Q1", "Q2","Q3","Q4","Q5","Q6", "Q7")

data %>% select(all_of(risk_rej_cols))


# Example usage:
missing_summary_table <-  summarize_missingness(data, risk_rej_cols)

# Display the table (using kable for a simple output)
missing_summary_table %>% knitr::kable()

# 9.	Check that values for QID30:Age fall between 40-74? (min/max)

# Example usage:
columns_to_summarize <-  c("QID30")  # Replace with your column names
summary_table <-  summarize_statistics(data, columns_to_summarize)

# View the summary table
summary_table %>% knitr::kable()




