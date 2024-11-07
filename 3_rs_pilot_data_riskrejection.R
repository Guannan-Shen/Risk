library("tidyverse")
library("magrittr")
source("./basic.R")
source("./global_vars.R")

df <- readRDS("../data/processing/rs_pilot_final_dataset.rds")

# "abs_risk_lifetime_rounded"

df %<>%
  mutate(
    abs_risk_rounded = as.numeric(abs_risk_rounded),
    abs_risk_lifetime_rounded = as.numeric(abs_risk_lifetime_rounded),
    RReject1 = as.numeric(RReject1)
  ) %>%
  mutate(
    # Create RReject1_categorical based on comparison with abs_risk_rounded
    RReject1_categorical = case_when(
      RReject1 < abs_risk_rounded ~ "1",
      # RReject1 is less than abs_risk_rounded
      RReject1 > abs_risk_rounded ~ "2",
      # RReject1 is greater than abs_risk_rounded
      RReject1 == abs_risk_rounded ~ "3"# RReject1 is equal to abs_risk_rounded
    ),
    # Create the difference score (RReject_diff)
    RReject_diff = RReject1 - abs_risk_rounded,
    # Create RReject_diff_categorical based on RReject_diff
    RReject_diff_categorical = case_when(RReject_diff < 0 ~ "1", # Negative difference
                                         RReject_diff > 0 ~ "2", # Positive difference
                                         RReject_diff == 0 ~ "3"# No difference
                                         )
    ) %>%
  mutate( across( c(RReject4, RReject5, RReject6, RReject7),
      ~  ifelse(. == 7, 5, .)  # Recode 7 to 5, keep other values the same
    ) )
    
    
# df %>% select(abs_risk_rounded) %>% arrange(desc(abs_risk_rounded))
# problem 1 2 3 4 7 not 1 2 3 4 5  
# table(df$RReject6)


library(expss)  # For unlab()

df_risk_modified <- df %>%
  mutate(
    across(
      c(RReject4, RReject5, RReject6, RReject7), 
      ~ case_when(
        . == 1 ~ 5,  # 1 becomes 5
        . == 2 ~ 4,  # 2 becomes 4
        . == 3 ~ 3,  # 3 stays the same
        . == 4 ~ 2,  # 4 becomes 2
        . == 5 ~ 1   # 5 becomes 1
      )
    )) %>%  mutate(RReject_diff = 
      unlab(RReject_diff)) %>%  mutate(RReject_diff = case_when(
  ResponseId == "R_1LUtAxnLeszwqkC" ~ 0,
  ResponseId == "R_28RFoTia7YU40yO" ~ 0,
  ResponseId == "R_3Mo4X2lgxig4ct9" ~ 984,
  TRUE ~ RReject_diff  # Keep the original value for all other rows
)) %>% mutate(Abs_RReject_diff = abs(RReject_diff)) %>%
  mutate(
    # Mean score for Subnum1 - Subnum3
    # Or using dplyr::select instead of across
    SubNum_mean = round(rowMeans(across(all_of(paste0("SubNum", 1:3))), 
                                 na.rm = TRUE), 2),
    # all_of(paste0("RReject", 4:7))
    RReject_mean = 
      rowMeans(across(all_of(paste0("RReject", 4:7)))) ) %>%  
  mutate(
      # Convert Experimental condition to factor with labels
      Condition = factor(`Experimental condition`, 
              levels = c(3, 1, 2),  # Reorder to match "Control", "Condition a", "Condition b"
              labels = c("Control", "Condition a", "Condition b")),
      # Ensure Age is numeric
      Age = as.numeric(Age),
      RReject3 = factor(
        RReject3, 
        levels = c(1, 2, 3), 
        labels = c(
          "higher than", 
          "about the same as", 
          "lower than")),
      abs_risk_relative_text = factor(
        abs_risk_relative_text, 
        levels = c("higher than", "about the same as", "lower than")
      )
    ) %>%
  mutate(
    # (a) Create 3 age groups: 40-49, 50-59, 60-74
    Age_group_3 = cut(
      Age,
      breaks = c(39, 49, 59, 74),  # Adjusted breaks starting at 39
      labels = c("40-49", "50-59", "60-74"),  
      right = TRUE  # Upper boundary is inclusive
    ),
    
    # (b) Create 2 age groups: 40-64 and 65-74
    Age_group_2 = cut(
      Age,
      breaks = c(39, 64, 74),  # Adjusted breaks starting at 39
      labels = c("40-64", "65-74"),
      right = TRUE
    )
  ) %>%
  mutate(
    # (a) Create 3 age groups: 40-49, 50-59, 60-74
    Age_group_3 = cut(
      Age,
      breaks = c(39, 49, 59, 74),  # Adjusted breaks starting at 39
      labels = c("40-49", "50-59", "60-74"),  
      right = TRUE  # Upper boundary is inclusive
    ),
    
    # (b) Create 2 age groups: 40-64 and 65-74
    Age_group_2 = cut(
      Age,
      breaks = c(39, 64, 74),  # Adjusted breaks starting at 39
      labels = c("40-64", "65-74"),
      right = TRUE
    )
  )  %>%
  mutate(
    # Create RReject_comp_1 with the desired 9 levels
    RReject_comp_1 = case_when(
      # Accurate estimates when both are the same
      RReject3 == "higher than" & abs_risk_relative_text == "higher than" ~ "accurate estimate",
      RReject3 == "about the same as" & abs_risk_relative_text == "about the same as" ~ "accurate estimate",
      RReject3 == "lower than" & abs_risk_relative_text == "lower than" ~ "accurate estimate",
      
      # Overestimate cases
      RReject3 == "higher than" & abs_risk_relative_text == "about the same as" ~ "overestimate, about the same as",
      RReject3 == "higher than" & abs_risk_relative_text == "lower than" ~ "very overestimate, lower than",
      
      # Underestimate cases
      RReject3 == "lower than" & abs_risk_relative_text == "about the same as" ~ "underestimate, about the same as",
      RReject3 == "lower than" & abs_risk_relative_text == "higher than" ~ "very underestimate, higher than",
      
      # New cases for "about the same as"
      RReject3 == "about the same as" & abs_risk_relative_text == "higher than" ~ "underestimate, higher than",
      RReject3 == "about the same as" & abs_risk_relative_text == "lower than" ~ "overestimate, lower than",
      
      # Default case (for missing or unclassified combinations)
      TRUE ~ NA_character_
    )
  ) %>%
  # Order the levels of RReject_comp_1
  mutate(
    RReject_comp_1 = factor(
      RReject_comp_1,
      levels = c(
        "very underestimate, higher than",
        "underestimate, about the same as",
        "underestimate, higher than",
        "accurate estimate",
        "overestimate, lower than",
        "overestimate, about the same as",
        "very overestimate, lower than"
      )
    )) %>%
  mutate(
    # Create RReject_comp_2 with 5 levels
    RReject_comp_2 = case_when(
      # Accurate estimate conditions
      RReject3 == "higher than" & abs_risk_relative_text == "higher than" ~ "accurate estimate",
      RReject3 == "about the same as" & abs_risk_relative_text == "about the same as" ~ "accurate estimate",
      RReject3 == "lower than" & abs_risk_relative_text == "lower than" ~ "accurate estimate",
      
      # Overestimate conditions (combined)
      RReject3 == "higher than" & abs_risk_relative_text == "about the same as" ~ "overestimate",
      RReject3 == "about the same as" & abs_risk_relative_text == "lower than" ~ "overestimate",
      
      # Very overestimate condition
      RReject3 == "higher than" & abs_risk_relative_text == "lower than" ~ "very overestimate",
      
      # Underestimate conditions (combined)
      RReject3 == "lower than" & abs_risk_relative_text == "about the same as" ~ "underestimate",
      RReject3 == "about the same as" & abs_risk_relative_text == "higher than" ~ "underestimate",
      
      # Very underestimate condition
      RReject3 == "lower than" & abs_risk_relative_text == "higher than" ~ "very underestimate",
      
      # Default case for missing or unclassified combinations
      TRUE ~ NA_character_
    )
  ) %>%
  # Order the levels of RReject_comp_2
  mutate(
    RReject_comp_2 = factor(
      RReject_comp_2,
      levels = c(
        "very underestimate",
        "underestimate",
        "accurate estimate",
        "overestimate",
        "very overestimate"
      )
    )
  ) %>%
  mutate(
    # Create RReject_comp_3 with 3 levels
    RReject_comp_3 = case_when(
      # Accurate estimate conditions
      RReject3 == "higher than" & abs_risk_relative_text == "higher than" ~ "accurate estimate",
      RReject3 == "about the same as" & abs_risk_relative_text == "about the same as" ~ "accurate estimate",
      RReject3 == "lower than" & abs_risk_relative_text == "lower than" ~ "accurate estimate",
      
      # Overestimate conditions (combined)
      RReject3 == "higher than" & abs_risk_relative_text == "about the same as" ~ "overestimate",
      RReject3 == "about the same as" & abs_risk_relative_text == "lower than" ~ "overestimate",
      RReject3 == "higher than" & abs_risk_relative_text == "lower than" ~ "overestimate",
      
      # Underestimate conditions (combined)
      RReject3 == "lower than" & abs_risk_relative_text == "about the same as" ~ "underestimate",
      RReject3 == "about the same as" & abs_risk_relative_text == "higher than" ~ "underestimate",
      RReject3 == "lower than" & abs_risk_relative_text == "higher than" ~ "underestimate",
      
      # Default case for missing or unclassified combinations
      TRUE ~ NA_character_
    )
  ) %>%
  # Order the levels of RReject_comp_3
  mutate(
    RReject_comp_3 = factor(
      RReject_comp_3,
      levels = c("underestimate", "accurate estimate", "overestimate")
    )
  ) %>%
  mutate(
    # Create a binary variable based on RReject_diff
    RReject_diff_binary = if_else(RReject_diff >= 90, 
                                  "RReject_diff >= 90", "RReject_diff < 90")
  )



# TODO: create age group
# (a) 3 age groups 40-49; 50-59; 60-74, and 
# (b) 2 age groups 40-64 and 65-74
  
df_risk_modified$RReject1[df_risk_modified$ResponseId 
                          == "R_1LUtAxnLeszwqkC"] <- 8
df_risk_modified$RReject1[df_risk_modified$ResponseId 
                          == "R_28RFoTia7YU40yO"] <- 11
df_risk_modified$RReject1[df_risk_modified$ResponseId 
                          == "R_3Mo4X2lgxig4ct9"] <- 1000

# df_tmp <- df %>%
#   mutate(
#     Race = case_when(
#       Race == 1 ~ "American Indian or Alaska Native",
#       Race == 2 ~ "Asian or Asian American",
#       Race == 3 ~ "Black or African American",
#       Race == 4 ~ "Native Hawaiian or Other Pacific Islander",
#       Race == 5 ~ "White or European American",
#       Race %!in% 1:5 | is.na(Race) ~ "Others"  # Combine 6, 7, other values, and NAs as "Others"
#     ),
#     Race = factor(Race, levels = c(
#       "American Indian or Alaska Native", 
#       "Asian or Asian American", 
#       "Black or African American", 
#       "Native Hawaiian or Other Pacific Islander", 
#       "White or European American", 
#       "Others"
#     ))
#   )

df_risk_modified %<>%  mutate(
  `Race/Ethnicity` = factor(
    `Race/Ethnicity`,
    levels = 1:5,
    labels = c(
      "White",
      "African or African American / Black",
      "Hispanic/Latinx",
      "Asian or Asian American",
      "American Indian or Alaskan Native"
    ))) %>%
  mutate(
    across(all_of(c(grp_medmis_cols, health_sysdis_cols)), 
           as.numeric)
  ) %>%
  mutate(
    across(
      c(HealthSysDis1, HealthSysDis2, HealthSysDis4, HealthSysDis7),
      ~ unlab(.)
    )) %>%
  mutate(
    across(
      c(HealthSysDis1, HealthSysDis2, HealthSysDis4, HealthSysDis7),
      ~ case_when(
        . == 5 ~ 1,
        . == 4 ~ 2,
        . == 3 ~ 3,
        . == 2 ~ 4,
        . == 1 ~ 5,
        TRUE ~ .  # Keep missing values as they are
      )))  %>%
  mutate(
    # Sum of GroupMedMis1 - GroupMedMis6
    GroupMed_Suspicion = rowSums(dplyr::select(., 
                      all_of(grp_medmis_cols[1:6])), na.rm = TRUE),
    
    # Sum of GroupMedMis7 - GroupMedMis9
    GroupMed_Disparities = rowSums(dplyr::select(., 
                     all_of(grp_medmis_cols[7:9])), na.rm = TRUE),
    
    # Sum of HealthSysDis1 - HealthSysDis4
    HealthSys_Competence = rowSums(dplyr::select(., 
                        all_of(health_sysdis_cols[1:4])), na.rm = TRUE),
    
    # Sum of HealthSysDis5 - HealthSysDis9
    HealthSys_Values = rowSums(dplyr::select(., 
                        all_of(health_sysdis_cols[5:9])), na.rm = TRUE)
  ) %>%
  # Convert all 0 values in the new columns to NA
  mutate(
    across(c(GroupMed_Suspicion, GroupMed_Disparities, HealthSys_Competence, HealthSys_Values), 
           ~ na_if(., 0))
  ) %>%
  rename(Race_Ethnicity = `Race/Ethnicity`)


saveRDS(df, "../data/processing/rs_pilot_risk_rejection_dataset.rds")

saveRDS(df_risk_modified, 
 "../data/processing/rs_pilot_risk_rejection_modified_dataset.rds")

write.csv(df,
          "../data/processing/rs_pilot_risk_rejection_dataset.csv",
          row.names = FALSE)

# subset of dataset to view 
# includes “ResponseID”, “abs_risk_rounded”,  
# “RReject1”, a difference score (RReject1 – abs_risk_rounded) , and “RReject2”
sub_view_df <- df %>% select(ResponseId, abs_risk_rounded, RReject1, RReject_diff, RReject2)

write.csv(sub_view_df,
          "../data/processing/rs_pilot_rrej1_diff_rrej2_dataset.csv",
          row.names = FALSE)

#  “ResponseID”, “abs_risk_rounded”, “Abs_risk_age_group_rounded”, difference scores (RReject1-abs_risk_rounded), “RReject3” and “abs_risk_relative_text”
    
sub_view2_df <- df %>% select(ResponseId, abs_risk_rounded, 
                              abs_risk_age_group_rounded, RReject_diff, 
                              RReject3, abs_risk_relative_text)

write.csv(sub_view2_df,
          "../data/processing/rs_pilot_riskAge_diff_rrej3_rtext_dataset.csv",
          row.names = FALSE)

# df$abs_risk_age_group_rounded
# %>%
#   modify_spanning_header(c("stat_1", "stat_2", "stat_3") ~ 
#                            "**Experimental Conditions**")

