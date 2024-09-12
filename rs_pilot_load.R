# RS means risk_skepticism
source("./basic.R")
source("./load_SPSS_data.R")
source("./helper.R")
source("./data_clean_time.R")
source("./table_helper.R")
source("./missing_helper.R")

library("tidyverse")
# exploratory analysis  
file_path = "../data/raw_data/Risk Skepticism Long Survey for Pilot Test_September 6, 2024_09.24.csv"

# header_data = read_csv(file_path, skip = 0, n_max = 2, col_names = FALSE)
# glimpse(header_data)
# is_unique(header_data[1,])
# is_unique(header_data[2,])
# # DONE: compare new dataset and old dataset has the same variable labels or not
# fp_old = "../data/raw_data/Pilot Risk soft launch data_numeric values.csv"
# hd_old = read_csv(fp_old, skip = 0, n_max = 2, col_names = FALSE)
# diff_labels = as.vector(!header_data[2,] == hd_old[2,])
# glimpse(header_data[2, diff_labels])
# glimpse(hd_old[2, diff_labels])
# glimpse(header_data[1, diff_labels])
# glimpse(hd_old[1, diff_labels])

# In general, the soft launch of the pilot data was stacked to the whole pilot data
# the whole dataset has a much meaningful non-duplicated names, 
# but the variable labels still work as a better tag of the variables 
cleaned_data = load_var_label(file_path)
saveRDS(cleaned_data, "../data/processing/rs_pilot_labels_processed.rds")

# data <- readRDS("../data/processing/rs_pilot_labels_processed.rds")
# 
# glimpse(data)
# 
# 
# dim(data)[1]

# TODO: compare different variable names between rs_pilot and rs soft launch
# TODO: generate useful col names vectors in global_vars.R

# diff_names = as.vector(!header_data[1,] == hd_old[1,])
# glimpse(header_data[1, diff_names])
# glimpse(hd_old[1, diff_names])
# 
# table(data$RRecall1, useNA = "ifany")
# table(data$RiskComp1, useNA = "ifany")
# 
# old_data <- readRDS("../data/processing/data_labels.rds")
# table(old_data$Q1.11, useNA = "ifany")
# 
# table(data$GroupMedMis1, useNA = "ifany")
# 
# table(data$`Pre-estimate`, useNA = "ifany")
# table(old_data$`Experimental condition`, useNA = "ifany")
# 
# table(data$`RiskCon Info Seeking`, useNA = "ifany")

# TODO: screening by screening columns 
raw_data <- readRDS("../data/processing/rs_pilot_labels_processed.rds")
raw_data %<>% create_screen_label(screening_cols)

write_csv(raw_data %>% filter(is.na(Pass_screening)), 
          "../data/processing/rs_pilot_missing_screening.csv")

saveRDS(raw_data %>% filter(!is.na(Pass_screening)), 
                            "../data/processing/rs_pilot_have_screening.rds")

saveRDS(raw_data %>% filter(Pass_screening == "No"), 
        "../data/processing/rs_pilot_fail_screening.rds")
saveRDS(raw_data %>% filter(Pass_screening == "Yes"), 
        "../data/processing/rs_pilot_pass_screening.rds")

# there are missingness in experimental condition, indicating not finished
pass_screening_df <- readRDS("../data/processing/rs_pilot_pass_screening.rds")

pass_screening_df %<>% generate_missingness_flag("Experimental condition")

saveRDS(pass_screening_df %>%
          filter(!is.na(`Experimental condition`)),
         "../data/processing/rs_pilot_have_experiment.rds")

data <- readRDS("../data/processing/rs_pilot_have_experiment.rds")

saveRDS(data %>% filter(!(is.na(`Pre-estimate`) 
                         &`Experimental condition` != 3)),
        "../data/processing/rs_pilot_good_preestimate.rds")

### TODO: important, check missingness pattern, and filtering dataset 

raw_df <- readRDS("../data/processing/rs_pilot_good_preestimate.rds")

raw_df %<>% mutate(total_missing_prop_obs = 
                     round(rowMeans(is.na(.)), 4))

missing_flag_df <- raw_df %>% select(all_of(c( risks_cols[2: length(risks_cols)], 
                                               risk_rej_cols))) %>% 
  mutate(risks_missing_prop_obs = 
           round(rowMeans(is.na(.)), 4))

raw_df %<>% mutate(risks_missing_prop_obs = missing_flag_df$risks_missing_prop_obs)

# TODO: important save dataset, raw_df with missingness flag 

saveRDS(raw_df, "../data/processing/rs_pilot_good_esti_missing_flag.rds")

# check number of observations with worse missingness
no_worst_missing_df <- raw_df %>% filter(risks_missing_prop_obs <= 0.25)

saveRDS(no_worst_missing_df,
          "../data/processing/rs_pilot_no_worst_missing.rds")





