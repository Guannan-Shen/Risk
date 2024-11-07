library("tidyverse")
source("./basic.R")


df <- readRDS("../data/processing/rs_pilot_final_dataset.rds") 

att <- c("AttCheck1", "AttCheck2", "Use_of_data")
cols <- c("Duration (in seconds)")

median_duration <- median(df$`Duration (in seconds)`)

## prepare variables for RQ4
df %<>% mutate(
  # Create Speeder flag (1 = Speeder, 0 = Not Speeder)
  Speeder = if_else(`Duration (in seconds)` < 
                      0.5 * median_duration, 0, 1),
  
  # Create Attflag1 (1 = pass, 0 = fail for AttCheck1)
  Attflag1 = if_else(AttCheck1 == 4, 1, 0),
  
  # Create Attflag2 (1 = pass, 0 = fail for AttCheck2)
  Attflag2 = if_else(AttCheck2 == 4, 1, 0),
  
  # Create Attcount1 (sum of Attflag1 and Attflag2)
  Attcount1 = Attflag1 + Attflag2,
  
  # Create Attcount2 (sum of Attflag1, Attflag2, and Use_of_data)
  # Assuming Use_of_data is also a flag where 1 means "pass" and 0 means "fail"
  Attcount2 = Attflag1 + Attflag2 + Use_of_data,
  
  # Create Attcount3 (sum of Attflag1, Attflag2, Use_of_data, and Speeder)
  Attcount3 = Attflag1 + Attflag2 + Use_of_data + Speeder
) %>%
  mutate(
    # Create Attatleast1: 1 if passed at least one attention check, 0 otherwise
    Attatleast1 = if_else(Attcount1 >= 1, 1, 0),
    Attcount4 = Use_of_data + Speeder + Attatleast1
    ) %>% 
  mutate(
    # Add labels for Speeder (1 = Speeder, 0 = Not Speeder)
    Speeder = factor(Speeder, levels = c(0, 1), labels = c("Fail", "Pass")),
    
    # Add labels for Attflag1 (1 = pass, 0 = fail for AttCheck1)
    Attflag1 = factor(Attflag1, levels = c(0, 1), labels = c("Fail", "Pass")),
    
    # Add labels for Attflag2 (1 = pass, 0 = fail for AttCheck2)
    Attflag2 = factor(Attflag2, levels = c(0, 1), labels = c("Fail", "Pass")),
    
    # Add labels for Attcount1 (0 = Fail both, 1 = Pass 1, 2 = Pass both)
    Attcount1 = factor(Attcount1, levels = c(0, 1, 2), 
                       labels = c("Fail both", "Pass one", "Pass both")),
    
    # Add labels for Attcount2 (0 = Fail all, 1 = Pass 1, 2 = Pass 2, 3 = Pass all)
    Attcount2 = factor(Attcount2, levels = c(0, 1, 2, 3), 
                       labels = c("Fail all", "Pass one", "Pass two", "Pass all")),
    Attcount3 = factor(Attcount3, levels = c(0, 1, 2, 3, 4), 
                       labels = c("Fail all", "Pass one", "Pass two", "Pass three", "Pass all")),
    
    # Convert Attcount4 to a factor with meaningful labels
    Attcount4 = factor(Attcount4, levels = c(0, 1, 2, 3), 
                       labels = c("Fail all", "Pass one", "Pass two", "Pass all")), 
    
    
    # Add labels for Use_of_data (1 = Pass, 0 = Fail)
    Use_of_data = factor(Use_of_data, levels = c(0, 1), labels = c("Not use data", "Use data"))
  )


rs4_vars <- c("Use_of_data", "Speeder", "Attflag1", 
              "Attflag2", "Attcount1", "Attcount2")

# table(df[["Use_of_data"]], df[[ "Speeder"]], useNA = "ifany")


saveRDS(df, "../data/processing/rs_pilot_rq4_dataset.rds")

write.csv(df,
          "../data/processing/rs_pilot_rq4_dataset.csv",
          row.names = FALSE)

# is_unique(colnames(df))
# colnames(df)

library(expss)  # For  # For unlab()
# TODO: replace all missing values as 0
df <- readRDS("../data/processing/rs_pilot_final_dataset.rds") 
median_duration <- median(df$`Duration (in seconds)`)
## prepare variables for RS4
df %<>% # Remove labels from Use_of_data)
  mutate(Use_of_data = unlab(Use_of_data))  %>% 
  
  mutate(
  # Create Speeder flag (1 = Speeder, 0 = Not Speeder)
  Speeder = if_else(`Duration (in seconds)` < 
                      0.5 * median_duration, 0, 1),
  
  # Create Attflag1 (1 = pass, 0 = fail for AttCheck1)
  Attflag1 = if_else(AttCheck1 == 4, 1, 0),
  Attflag1 = replace_na(Attflag1, 0),  # Replace NA with 0
  
  # Create Attflag2 (1 = pass, 0 = fail for AttCheck2)
  Attflag2 = if_else(AttCheck2 == 4, 1, 0),
  Attflag2 = replace_na(Attflag2, 0),  # Replace NA with 0
  
  Use_of_data = replace_na(Use_of_data, 0), 
  
  # Create Attcount1 (sum of Attflag1 and Attflag2)
  Attcount1 = Attflag1 + Attflag2,
  
  # Create Attcount2 (sum of Attflag1, Attflag2, and Use_of_data)
  # Assuming Use_of_data is also a flag where 1 means "pass" and 0 means "fail"
  Attcount2 = Attflag1 + Attflag2 + Use_of_data,
  
  # Create Attcount3 (sum of Attflag1, Attflag2, Use_of_data, and Speeder)
  Attcount3 = Attflag1 + Attflag2 + Use_of_data + Speeder
) %>%
  mutate(
    # Create Attatleast1: 1 if passed at least one attention check, 0 otherwise
    Attatleast1 = if_else(Attcount1 >= 1, 1, 0),
    Attcount4 = Use_of_data + Speeder + Attatleast1
  ) %>% 
  mutate(
    # Add labels for Speeder (1 = Speeder, 0 = Not Speeder)
    Speeder = factor(Speeder, levels = c(0, 1), labels = c("Fail", "Pass")),
    
    # Add labels for Attflag1 (1 = pass, 0 = fail for AttCheck1)
    Attflag1 = factor(Attflag1, levels = c(0, 1), labels = c("Fail", "Pass")),
    
    # Add labels for Attflag2 (1 = pass, 0 = fail for AttCheck2)
    Attflag2 = factor(Attflag2, levels = c(0, 1), labels = c("Fail", "Pass")),
    
    # Add labels for Attcount1 (0 = Fail both, 1 = Pass 1, 2 = Pass both)
    Attcount1 = factor(Attcount1, levels = c(0, 1, 2), 
                       labels = c("Fail both", "Pass one", "Pass both")),
    
    # Add labels for Attcount2 (0 = Fail all, 1 = Pass 1, 2 = Pass 2, 3 = Pass all)
    Attcount2 = factor(Attcount2, levels = c(0, 1, 2, 3), 
                       labels = c("Fail all", "Pass one", "Pass two", "Pass all")),
    Attcount3 = factor(Attcount3, levels = c(0, 1, 2, 3, 4), 
                       labels = c("Fail all", "Pass one", "Pass two", "Pass three", "Pass all")),
    
    # Convert Attcount4 to a factor with meaningful labels
    Attcount4 = factor(Attcount4, levels = c(0, 1, 2, 3), 
                       labels = c("Fail all", "Pass one", "Pass two", "Pass all")), 
    # Add labels for Use_of_data (1 = Pass, 0 = Fail)
    Use_of_data = factor(Use_of_data, levels = c(0, 1), labels = c("Not use data", "Use data"))
  )

saveRDS(df, "../data/processing/rs_pilot_rq4_nomissing_dataset.rds")



