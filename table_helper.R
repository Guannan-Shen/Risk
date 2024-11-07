library(tidyverse)
library(kableExtra)
library(gt)

source("./helper.R")  

# Function to create a cross-tabulation and format it for PDF output
# Caption font size is not set by rmarkdown yaml header fontsize: 12pt
# TODO: may find more at https://apreshill.github.io/data-vis-labs-2018/05-tables.html#1_goals_for_lab_05
create_pdf_table = function(tab, caption, row_names = FALSE, 
                            label = FALSE, font_size = 12) {
  # Generate a table formatted for PDF using kableExtra
  # if(caption != ""){
  #   formatted_caption = gsub("^(.*)$",
  #                            paste0("\\\\", font_size, "{\\1}"), caption)
  # }
  if(label){
    row_label = names(as.data.frame(tab))[1]
    col_label = names(as.data.frame(tab))[2]
  }
  if(row_names){
    tab = cbind(row.names(tab), tab)
  }
  pdf_table = tab %>%
    # LaTeX package booktabs for publication-quality tables
    kable("latex", booktabs = TRUE, 
          caption = caption) %>%
    kable_styling(latex_options = c("hold_position", "striped"), 
                  font_size = font_size)
  
  if(label){
    pdf_table = pdf_table %>%
      # the label and the span
      # must set names
      # This construction of c("my header" = 7) won't work
      # if your headers are constructed on the fly
      # https://stackoverflow.com/questions/64414334/kableextra-how-does-the-add-header-above-function-work
      add_header_above(header = setNames(c(1, ncol(tab) - 1),
                                         c(row_label, col_label)))
  }
  
  return(pdf_table)
}

# 

# Define the function with counts and percentages
# Done: this is the function for frequency tables of only one variables 
freq_html_table <- function(data, column_name) {
  
  # Create a frequency table for the specified column
  freq_table <- table(data[[column_name]], useNA = "ifany")
  
  # with sum of margins 
  margins_table <- addmargins(freq_table)
  
  # Calculate the percentage
  perc_table <- prop.table(freq_table) * 100
  
  # Combine counts and percentages into a data frame
  combined_df <- data.frame(
    grp_levels = names(margins_table),  # Categorical levels
    Count = as.numeric(margins_table),        # Frequency counts
    Percentage = c(as.numeric(perc_table), 100)    # Percentage values
  )
  
  # Generate the gt table
  gt_table <- combined_df %>%
    gt::gt() %>%
    tab_header(
      title = paste("Frequency and Percentage of", column_name)
    ) %>%
    cols_label(
      grp_levels = column_name,
      Count = "Frequency Count",
      Percentage = "Percentage (%)"
    ) %>%
    fmt_number(
      columns = vars(Percentage),
      decimals = 3
    ) %>%
    tab_style(
      style = list(
        cell_text(weight = "bold")
      ),
      locations = cells_column_labels()
    )
  
  return(gt_table)
}



# generate html table based on gt::gt() from tibble, such as from summarize_missingness
create_basic_html_table <- function(df, title) {
  # Generate the gt table
  gt_table <- df %>%
    gt::gt() %>%
    tab_header(
      title = title
    )  %>%
    tab_style(
      style = list(
        cell_text(weight = "bold")
      ),
      locations = cells_column_labels()
    )
  
  # Return the gt table for HTML display
  return(gt_table)
}

# Function to create an HTML cross-frequency table with counts only
cross_freq_html_table_counts <- function(data, column1, column2) {
  
  # Create a contingency table for the two specified columns
  contingency_table <- table(data[[column1]], data[[column2]], useNA = "ifany")
  
  # Add row and column sums (margins)
  margins_table <- addmargins(contingency_table)
  
  # Convert table to a matrix format for easy use in gt::gt()
  table_matrix <- as.data.frame.matrix(margins_table)
  
  # Set row names to a separate column to keep them as row labels
  table_matrix <- cbind(Row = rownames(table_matrix), table_matrix)
  
  # FIX: missing colnames due to NA
  # Fix column names by replacing empty column names with "NA"
  colnames(table_matrix)[is.na(colnames(table_matrix))] <- "NA"
  rownames(table_matrix)[is.na(rownames(table_matrix))] <- "NA"
  
  # Generate the gt table
  gt_table <- table_matrix %>%
    # gt() %>%
    gt::gt(rowname_col = "Row") %>%  # Set rowname_col to preserve the row labels
    tab_header(
      title = paste("Cross-Frequency Table Between", column1, "and", column2)
    ) %>%
    tab_spanner(
      label = column2,  # Group the column headers under the second variable
      columns = everything(),
      id = paste0("spanner_", column2)  # Provide a unique spanner ID
    ) %>%
    tab_stubhead(label = column1) %>%  # Add the name of column1 to the row header
    tab_style(
      style = list(
        cell_text(weight = "bold")
      ),
      locations = cells_column_labels()
    )
  
  return(gt_table)
}

# TODO: batch rendering problem in Rmarkdown Quarto
# the outputs generated within that loop are not 
# automatically displayed in the final document 
# unless explicitly printed in a way that the document renderer can interpret. 
# This is especially true for outputs like gt() tables, 
# which are S3 objects that need to be properly handled to render in the document.

generate_column_report <- function(col_name, df_name) {
  # Create a temporary file for the child document
  tmp_file <- tempfile(fileext = ".qmd")
  
  # Write the code to the temporary file
  cat(
    "---\n",
    "execute:\n",
    "  echo: false\n",
    "---\n\n",
    "#### Frequency Table for ", col_name, "\n\n",
    "```{r}\n",
    df_name, "%>% freq_html_table('", col_name, "')\n",
    "```\n\n",
    "#### Descriptive Summary of ", col_name, "\n\n",
    "```{r}\n",
    df_name, "%>% summarize_statistics('", col_name, "') %>% \n",
    "  create_basic_html_table(title = 'Descriptive Summary of ", col_name, "')\n",
    "```\n",
    file = tmp_file, sep = ""
  )
  
  # Return the path to the temporary file
  return(tmp_file)
}

## generate simple freq html table report

generate_sim_freq_report <- function(col_name, df_name) {
  # Create a temporary file for the child document
  tmp_file <- tempfile(fileext = ".qmd")
  
  # Write the code to the temporary file
  cat(
    "---\n",
    "execute:\n",
    "  echo: false\n",
    "---\n\n",
    "### Frequency Table for ", col_name, "\n\n",
    "```{r}\n",
    df_name, "%>% freq_html_table('", col_name, "')\n",
    file = tmp_file, sep = ""
  )
  
  # Return the path to the temporary file
  return(tmp_file)
}

# Function to create an HTML cross-frequency table with percentages only
cross_freq_html_table_percentages <- function(data, column1, column2) {
  
  # Create a contingency table for the two specified columns
  contingency_table <- table(data[[column1]], data[[column2]], useNA = "ifany")
  
  # Calculate the percentages based on the total counts
  percentages_table <- prop.table(contingency_table) * 100
  
  # Add row and column sums (margins) to percentages
  margins_table <- addmargins(percentages_table)
  
  # Convert table to a matrix format for easy use in gt::gt()
  table_matrix <- as.data.frame.matrix(margins_table)
  
  # Set row names to a separate column to keep them as row labels
  table_matrix <- cbind(Row = rownames(table_matrix), table_matrix)
  
  # Fix missing column names by replacing empty column names with "NA"
  colnames(table_matrix)[is.na(colnames(table_matrix))] <- "NA"
  rownames(table_matrix)[is.na(rownames(table_matrix))] <- "NA"
  
  # Generate the gt table with percentages only
  gt_table <- table_matrix %>%
    gt::gt(rowname_col = "Row") %>%  # Set rowname_col to preserve the row labels
    tab_header(
      title = paste("Percentage (%) Cross-Frequency Table Between", column1, "and", column2)
    ) %>%
    tab_spanner(
      label = column2,  # Group the column headers under the second variable
      columns = everything(),
      id = paste0("spanner_", column2)  # Provide a unique spanner ID
    ) %>%
    tab_stubhead(label = column1) %>%  # Add the name of column1 to the row header
    tab_style(
      style = list(
        cell_text(weight = "bold")
      ),
      locations = cells_column_labels()
    ) %>%
    fmt_number(
      columns = everything(),  # Format all percentage columns
      decimals = 2  # Set the number of decimals to display
    )
  
  return(gt_table)
}


generate_desc_summary_report <- function(col_name, df_name) {
  # Create a temporary file for the child document
  tmp_file <- tempfile(fileext = ".qmd")
  
  # Write the code to the temporary file
  cat(
    "---\n",
    "execute:\n",
    "  echo: false\n",
    "---\n\n",
    "#### Descriptive Summary of ", col_name, "\n\n",
    "```{r}\n",
    df_name, "%>% summarize_statistics('", col_name, "') %>% \n",
    "  create_basic_html_table(title = 'Descriptive Summary of ", col_name, "')\n",
    "```\n",
    file = tmp_file, sep = ""
  )
  
  # Return the path to the temporary file
  return(tmp_file)
}

# Function to process each column and render HTML tables
# Ready for purrr, functional programming 
# process_and_render <- function(df, col_name) {
#   # Render the frequency HTML table
#   freq_html <- freq_html_table(df, col_name)
#   
#   # Render the summary statistics HTML table
#   summary_html <- summarize_statistics(df, col_name) %>%
#     create_basic_html_table(paste("Descriptive Summary for", col_name))
#   
#   # Print the tables (so they render in the output)
#   # # This approach, tables are not rendered correctly in quarto html format
#   # purrr::walk(col_names, process_and_render, df = inclusive_df)
#   # print(freq_html)
#   # print(summary_html)
#   # Return both tables as a list
#   return(list(freq_html = freq_html, summary_html = summary_html))
# }


# # 
# contingency_table <- table(pass_screening_df[["missingness_Experimental condition"]],
#                            pass_screening_df[["Finished"]], useNA = "ifany")
# 
# margins_table <- addmargins(contingency_table)
# # TODO: as.data.frame() vs as.data.frame.matrix()
# # as.data.frame.matrix(margins_table)

# 
# contingency_table <- table(data[["Experimental condition"]],
#                            data[["Pre-estimate"]], useNA = "ifany")
# 
# # Add row and column sums (margins)
# margins_table <- addmargins(contingency_table)
# 
# # Convert table to a matrix format for easy use in gt()
# table_matrix <- as.data.frame.matrix(margins_table)
# is.na(colnames(table_matrix))
# 
# colnames(table_matrix)[is.na(colnames(table_matrix))] <- "NA"
# 
# colnames(table_matrix)
