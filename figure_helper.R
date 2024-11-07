library(ggplot2)

create_histogram <- function(df, variable_name, bin_breaks, title) {
  ggplot(df, aes(x = !!sym(variable_name) )) +
    geom_histogram(breaks = bin_breaks, fill = "white", color = "black") +
    labs(title = title, x = variable_name, y = "Count") +
    theme_minimal()
}

# TODO: boxplot with scatter dots, RReject4 - 7 categories and abs(RReject_diff) 
# TODO: violin plot 

create_boxplot_with_dots <- function(df, group_var, num_var) {
  ggplot(df, aes(x = as.factor(!!sym(group_var)), 
                 # TODO: abs() is not necessary
                 y = !!sym(num_var))) +
    # not showing outliers
    geom_boxplot(outlier.shape = NA, fill = "white", color = "black") +  # Boxplot without outliers
    geom_jitter(width = 0.2, color = "darkgrey", alpha = 0.8) +  # Scatter dots
    labs(title = paste0("Boxplot of ", num_var, 
                        " (Grouped by: ", group_var, " )"), 
         x = paste0(group_var), 
         y = paste0(num_var)) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 16),  # Adjust title size
          axis.title.x = element_text(size = 14, 
                                      # TODO: the x axis text tricky to adjust 
                                      vjust = 7),  # Adjust x-axis label size
          axis.title.y = element_text(size = 14),  # Adjust y-axis label size
          # TODO: the x axis text tricky to adjust 
          axis.text.x = element_text(size = 12, angle = 30, 
                                     vjust = 0.7),   # Adjust x-axis tick label size
          axis.text.y = element_text(size = 12)    # Adjust y-axis tick label size
    )
}


create_violin_plot <- function(df, group_var, num_var) {
  ggplot(df, aes(x = as.factor(!!sym(group_var)), 
                 # TODO: abs() is not necessary
                 y = !!sym(num_var))) +
    # trim If TRUE (default), trim the tails of the violins to the range of the data
    geom_violin(trim = TRUE, fill = "white", color = "black") +  # Violin plot
    # geom_jitter(width = 0.2, color = "darkgrey", alpha = 0.8) +  # Scatter dots
    labs(title = paste0("Violin plot of ", num_var, 
                        " (Grouped by: ", group_var, " )"),  
         x = paste0(group_var), 
         y = paste0(num_var)) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 16),  # Adjust title size
          axis.title.x = element_text(size = 14, 
                                      # TODO: the x axis text tricky to adjust 
                                      vjust = 7),  # Adjust x-axis label size
          axis.title.y = element_text(size = 14),  # Adjust y-axis label size
          # TODO: the x axis text tricky to adjust 
          axis.text.x = element_text(size = 12, angle = 30, 
                                     vjust = 0.7),   # Adjust x-axis tick label size
          axis.text.y = element_text(size = 12)    # Adjust y-axis tick label size
    )
}

# scatter plot with linear trend 
create_scatter_with_trend <- function(df, var1, var2) {
  ggplot(df, aes(x = !!sym(var1), y = !!sym(var2))) +
    geom_point(color = "black", alpha = 0.7) +  # Scatter points
    geom_smooth(method = "lm", color = "red", se = FALSE) +  # Linear trend line
    labs(title = paste("Scatter Plot of", var1, "vs", var2),
         x = var1,
         y = var2) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 16),
          axis.title.x = element_text(size = 14), 
          axis.title.y = element_text(size = 14))
}



