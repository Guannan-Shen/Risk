library(tidyverse)
library(kableExtra)


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



