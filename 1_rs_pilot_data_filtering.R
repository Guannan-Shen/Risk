# RS means risk_skepticism
source("./basic.R")
source("./load_SPSS_data.R")
source("./helper.R")
source("./data_clean_time.R")
source("./table_helper.R")
source("./missing_helper.R")
source("./global_vars.R")

library("tidyverse")

# exploratory analysis
file_path = "../data/raw_data/Risk Skepticism Long Survey for Pilot Test_September 6, 2024_09.24.csv"

# In general, the soft launch of the pilot data was stacked to the whole pilot data
# the whole dataset has a much meaningful non-duplicated names,
# but the variable labels still work as a better tag of the variables
cleaned_data = load_var_label(file_path)
saveRDS(cleaned_data,
        "../data/processing/rs_pilot_labels_processed.rds")

# pilot data start with 703 subjects, with variable labels
raw_data <- readRDS("../data/processing/rs_pilot_labels_processed.rds")
raw_data %<>% create_screen_label(screening_cols)

# now has 624 obs
pass_screening_df <- raw_data %>% filter(Pass_screening == "Yes")
dim(pass_screening_df)

n_invalid <- pass_screening_df %>%
  filter(abs_risk_rounded == "{Invalid Expression}") %>% nrow()
message("There are ",
        n_invalid,
        " subjects have invalid expression of abs_risk_rounded")


df <- pass_screening_df %>% filter(if_all(all_of(columns_to_check), ~ !is.na(.))) %>%
  filter(abs_risk_rounded != "{Invalid Expression}")

dim(df)


# Select the subset of rows that were removed by the filter
removed_data <- pass_screening_df %>%
  filter(abs_risk_rounded == "{Invalid Expression}" |
           # Rows where abs_risk_rounded is NA or invalid
           if_any(all_of(columns_to_check), ~ is.na(.)))  # Rows with missing values in any of the specified columns)

dim(removed_data)

saveRDS(df, "../data/processing/rs_pilot_final_dataset.rds")
saveRDS(
  removed_data,
  "../data/processing/rs_pilot_removed_from_pass_screen_dataset.rds"
)

# TODO: care more about those who have no abs_risk_rounded, 
# TODO: and those who have in invalid abs_risk_rounded.
no_abs_risk_df <- pass_screening_df %>% filter(is.na(abs_risk_rounded)) %>% 
  select(ResponseId, Progress, all_of(risk_calcs), abs_risk_rounded)

invalid_abs_risk_df <-  pass_screening_df %>% filter(abs_risk_rounded == 
                            "{Invalid Expression}") %>% 
               select(ResponseId, Progress, all_of(risk_calcs), abs_risk_rounded)

saveRDS(no_abs_risk_df, "../data/processing/rs_pilot_no_abs_risk_df_dataset.rds")
saveRDS(invalid_abs_risk_df, "../data/processing/rs_pilot_invalid_abs_risk_df_dataset.rds")

write.csv(no_abs_risk_df,
          "../data/processing/rs_pilot_no_abs_risk_dataset.csv",
          row.names = FALSE)

write.csv(invalid_abs_risk_df,
          "../data/processing/rs_pilot_invalid_abs_risk_dataset.csv",
          row.names = FALSE)

# Save as CSV file
write.csv(df,
          "../data/processing/rs_pilot_final_dataset.csv",
          row.names = FALSE)
write.csv(
  removed_data,
  "../data/processing/rs_pilot_removed_from_pass_screen_dataset.csv",
  row.names = FALSE
)
