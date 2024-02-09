library("tidyverse") # This will load ggplot2, dplyr, readxl, and other essential packages
library("writexl")
library("boot")
library("readxl")

rm(list = ls())
# Set Working Directory
setwd("H:/My Drive/BIAS/bias/AK/MAIVE")
list.files()
excel_files <- list.files(pattern = "\\.xlsx$", ignore.case = TRUE)
list_of_dataframes <- lapply(excel_files, read_excel)

tile <- gsub("MAIVE_", "", excel_files)
names(list_of_dataframes) <- tools::file_path_sans_ext(basename(tile))
# Assuming excel_files is your list of data frames
list_of_dataframes <- lapply(names(list_of_dataframes), function(name) {
  df <- list_of_dataframes[[name]]
  new_colnames <- ifelse(colnames(df) == "Row_Names", 
                         "Row_Names", 
                         paste(name, colnames(df), sep = "_"))
  colnames(df) <- new_colnames
  return(df)
})
names(list_of_dataframes) <- tools::file_path_sans_ext(basename(tile))


Percent1 <- c( "allBE_1", "allFE_1", "allpooled_1")
Percent1 <- list_of_dataframes[Percent1]
Percent1 <- Reduce(function(x, y) merge(x, y, by = "Row_Names", all = TRUE), Percent1)
colnames(Percent1) <- gsub("*_1_*", "_", colnames(Percent1))

Percent2 <- c( "allBE_2", "allFE_2", "allpooled_2")
Percent2 <- list_of_dataframes[Percent2]
Percent2 <- Reduce(function(x, y) merge(x, y, by = "Row_Names", all = TRUE), Percent2)
colnames(Percent2) <- gsub("*_2_*", "_", colnames(Percent2))

Percent5 <- c( "allBE_5", "allFE_5", "allpooled_5")
Percent5 <- list_of_dataframes[Percent5]
Percent5 <- Reduce(function(x, y) merge(x, y, by = "Row_Names", all = TRUE), Percent5)
colnames(Percent5) <- gsub("*_5_*", "_", colnames(Percent5))


df_list <- list(Percent1 = Percent1, Percent2 = Percent2, Percent5 = Percent5)

# all_objects <- ls()
# rm(list = all_objects[all_objects != "df_list"])

# Define a function to create beta columns
create_beta_columns <- function(df) {
  df <- df %>%
    mutate(
      beta      = abs(allFE_E/allBE_E),
    )
  
  # Replace 0 and Inf with NA in beta columns
  df <- df %>%
    mutate(across(starts_with("beta"), ~na_if(.x, 0))) %>%
    mutate(across(starts_with("beta"), ~na_if(.x, Inf)))
  
  return(df)
}

# Apply the function to each data frame in the list
df_list <- map(df_list, create_beta_columns)

# Function to calculate the mean and its 95% CI
mean_ci <- function(x) {
  se <- sd(x, na.rm = TRUE) / sqrt(length(x))
  mean_x <- mean(x, na.rm = TRUE)
  ci <- qnorm(c(0.025, 0.975), mean = mean_x, sd = se)
  c(mean = mean_x, ci_lower = ci[1], ci_upper = ci[2])
}

# Function to calculate the median and its 95% CI using bootstrapping
median_ci <- function(x, n = 1000) {
  boot_obj <- boot(x, statistic = function(x, i) median(x[i]), R = n)
  ci <- boot.ci(boot_obj,conf = 0.95, type = "perc")$percent[4:5]
  c(median = median(x, na.rm = TRUE), ci_lower = ci[1], ci_upper = ci[2])
}

# Define a custom function to summarize the data with CIs and p-values
summarize_stats <- function(data, fe_f_col, be_f_col, fe_n_col, be_n_col, beta_col) {
  
  # Check if columns exist in the data
  columns_to_check <- c(fe_f_col, be_f_col, fe_n_col, be_n_col, beta_col)
  if(any(!columns_to_check %in% colnames(data))) stop("Column not found in the data.")
  
  # Filter and summarize data
  summary <- data %>%
    filter(!!sym(fe_f_col) > 10, !!sym(be_f_col) > 10, !!sym(fe_n_col) > 10, !!sym(be_n_col) > 10) %>%
    summarise(
      count = n(),
      median_stat = list(median_ci(!!sym(beta_col))),
      mean_stat = list(mean_ci(!!sym(beta_col)))
    ) %>%
    mutate(
      variable = beta_col,
      mean = mean_stat[[1]]['mean'],
      mean_ci_lower = mean_stat[[1]]['ci_lower'],
      mean_ci_upper = mean_stat[[1]]['ci_upper'],
      median = median_stat[[1]]['median'],
      median_ci_lower = median_stat[[1]]['ci_lower'],
      median_ci_upper = median_stat[[1]]['ci_upper']
    ) %>%
    select(-median_stat, -mean_stat)
  
  # Calculate p-values
  beta_values <- na.omit(data[[beta_col]])
  beta_values <- beta_values[is.finite(beta_values) & beta_values > 0 & beta_values != Inf & beta_values != -Inf]
  
  t_test_result <- t.test(beta_values, mu = 1, alternative = "greater")
  wilcox_test_result <- wilcox.test(beta_values, mu = 1, alternative = "greater", conf.int = TRUE)
  
  # Add p-values to the summary
  summary <- summary %>%
    mutate(
      mean_p_value = t_test_result$p.value,
      median_p_value = wilcox_test_result$p.value
    )
  
  return(summary)
}


# Create a function to apply summarize_stats to each set of conditions
apply_summary <- function(df, conditions) {
  map_dfr(conditions, ~summarize_stats(df, .x[1], .x[2], .x[3], .x[4], .x[5]))
}

# Define variable names for conditions
conditions <- list(
  c("allFE_F", "allBE_F", "allFE_Obs", "allBE_Obs", "beta")
)

# Apply the function to each data frame and collect results
results <- map(df_list, apply_summary, conditions)

# Combine all results into a single data frame with an identifier for each original data frame
final_results <- bind_rows(results, .id = "df_name")

# View the results
print(final_results)


library(xtable)
# Convert 'final_results' to an xtable object
xtable_object <- xtable(final_results)

# Print the xtable object to get the LaTeX code
print(xtable_object, include.rownames = FALSE, floating = FALSE)

# Capture the LaTeX code as a character vector
latex_code <- capture.output(print(xtable_object, include.rownames = FALSE, floating = FALSE))

# Write the LaTeX code to a file
writeLines(latex_code, "AK_all.tex")


library(openxlsx)
# Create a new workbook
wb <- createWorkbook()
# Add a sheet for each data frame
for (sheet_name in names(df_list)) {
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, df_list[[sheet_name]])
}
# Save the workbook to a file
saveWorkbook(wb, "multiple_sheets.xlsx", overwrite = TRUE)



# mean(df_list$Percent1[df_list$Percent1$allBE_F>10 & df_list$Percent1$allFE_F>10  & df_list$Percent1$beta_p > 0 & is.finite(df_list$Percent1$beta_p) & df_list$Percent1$pBE_Obs>10 & df_list$Percent1$pFE_Obs>10, ]$beta_p)


