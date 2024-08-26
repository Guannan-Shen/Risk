library(tidyverse)
library(expss)

# Step 1: Read in the data, skipping the first 3 rows 
data = read_csv("../data/raw_data/Pilot Risk soft launch data_numeric values.csv", 
                skip = 3, col_names = FALSE)

# Step 2: Read in the first two rows separately for variable names and labels
header_data = read_csv(file_path, skip = 0, n_max = 2, col_names = FALSE)

# Step 3: Extract variable names and labels
variable_names = header_data[1, ] %>% as.character() %>% make.unique()
variable_labels = header_data[2, ] %>% as.character()

# Step 4: Assign variable names to the data
names(data) = variable_names

labels_list = list()
for (i in seq_along(variable_names)) {
  labels_list[[variable_names[i]]] = variable_labels[i]
}
labels_list
# Step 5: Apply the variable labels using the expss package
data = apply_labels(data, labels_list)

sapply(data, var_lab)

