library("tidyverse")
source("./basic.R")


df <- readRDS("../data/processing/rs_pilot_final_dataset.rds") 

att <- c("AttCheck1", "AttCheck2", "Use_of_data")
cols <- c("Duration (in seconds)")

median_duration <- median(df$`Duration (in seconds)`)

## prepare variables for RS4
df %<>% mutate(
  # Create Speeder flag (1 = Speeder, 0 = Not Speeder)
  Speeder = if_else(`Duration (in seconds)` < 0.5 * median_duration, 1, 0),
  
  # Create Attflag1 (1 = pass, 0 = fail for AttCheck1)
  Attflag1 = if_else(AttCheck1 == 4, 1, 0),
  
  # Create Attflag2 (1 = pass, 0 = fail for AttCheck2)
  Attflag2 = if_else(AttCheck2 == 4, 1, 0),
  
  # Create Attcount1 (sum of Attflag1 and Attflag2)
  Attcount1 = Attflag1 + Attflag2,
  
  # Create Attcount2 (sum of Attflag1, Attflag2, and Use_of_data)
  # Assuming Use_of_data is also a flag where 1 means "pass" and 0 means "fail"
  Attcount2 = Attflag1 + Attflag2 + Use_of_data
) %>%
  mutate(
    # Add labels for Speeder (1 = Speeder, 0 = Not Speeder)
    Speeder = factor(Speeder, levels = c(0, 1), labels = c("Not Speeder", "Speeder")),
    
    # Add labels for Attflag1 (1 = pass, 0 = fail for AttCheck1)
    Attflag1 = factor(Attflag1, levels = c(0, 1), labels = c("Fail", "Pass")),
    
    # Add labels for Attflag2 (1 = pass, 0 = fail for AttCheck2)
    Attflag2 = factor(Attflag2, levels = c(0, 1), labels = c("Fail", "Pass")),
    
    # Add labels for Attcount1 (0 = Fail both, 1 = Pass 1, 2 = Pass both)
    Attcount1 = factor(Attcount1, levels = c(0, 1, 2), 
                       labels = c("Fail both", "Pass 1", "Pass both")),
    
    # Add labels for Attcount2 (0 = Fail all, 1 = Pass 1, 2 = Pass 2, 3 = Pass all)
    Attcount2 = factor(Attcount2, levels = c(0, 1, 2, 3), 
                       labels = c("Fail all", "Pass 1", "Pass 2", "Pass all")),
    
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
