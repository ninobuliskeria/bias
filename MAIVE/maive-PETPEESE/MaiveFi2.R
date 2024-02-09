library("tidyverse") # This will load ggplot2, dplyr, readxl, and other essential packages
library("writexl")
library("boot")
library("readxl")

rm(list = ls())
# Set Working Directory
setwd("H:/My Drive/BIAS/bias/MAIVE")
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


Percent1 <- c( "allBE_1", "allFE_1", "allpooled_1", "npBE_1", "npFE_1", "nppooled_1", "pBE_1",   "pFE_1", "ppooled_1", "wpBE_1",  "wpFE_1", "wppooled_1", "wpsBE_1", "wpsFE_1", "wpspooled_1")
Percent1 <- list_of_dataframes[Percent1]
Percent1 <- Reduce(function(x, y) merge(x, y, by = "Row_Names", all = TRUE), Percent1)
colnames(Percent1) <- gsub("*_1_*", "_", colnames(Percent1))

Percent2 <- c( "allBE_2", "allFE_2", "allpooled_2", "npBE_2", "npFE_2", "nppooled_2", "pBE_2",   "pFE_2", "ppooled_2", "wpBE_2",  "wpFE_2", "wppooled_2", "wpsBE_2", "wpsFE_2", "wpspooled_2")
Percent2 <- list_of_dataframes[Percent2]
Percent2 <- Reduce(function(x, y) merge(x, y, by = "Row_Names", all = TRUE), Percent2)
colnames(Percent2) <- gsub("*_2_*", "_", colnames(Percent2))

Percent5 <- c( "allBE_5", "allFE_5", "allpooled_5", "npBE_5", "npFE_5", "nppooled_5", "pBE_5",   "pFE_5", "ppooled_5", "wpBE_5",  "wpFE_5", "wppooled_5", "wpsBE_5", "wpsFE_5", "wpspooled_5")
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
      b_wpp      = abs(wppooled_E/ppooled_E),
      b_wpsp     = abs(wpspooled_E/ppooled_E),
      b_npp      = abs(nppooled_E/ppooled_E),
      
      b_wpp_FE   = abs(wpFE_E/pFE_E),
      b_wpsp_FE  = abs(wpsFE_E/pFE_E),
      b_npp_FE   = abs(npFE_E/pFE_E),
      
      b_wp_BE   = abs(wpBE_E/pBE_E),
      b_wps_BE  = abs(wpsBE_E/pBE_E),
      b_np_BE   = abs(npBE_E/pBE_E)
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

  t_test_result <- t.test(beta_values, mu = 1, alternative = "greater", conf.int = TRUE)
  wilcox_test_result <- wilcox.test(beta_values, mu = 1, alternative = "greater", conf.int = TRUE)

  # Add p-values to the summary
  summary <- summary %>%
    mutate(
      mean_p_value = t_test_result$p.value,
      median_p_value = wilcox_test_result$p.value,
      # mean_ci = t_test_result$conf.int,
      # median_ci = wilcox_test_result$conf.int
    )

  return(summary)
}

#====================

# summarize_stats <- function(data, fe_f_col, be_f_col, fe_n_col, be_n_col, beta_col) {
#   
#   # Check if columns exist in the data
#   columns_to_check <- c(fe_f_col, be_f_col, fe_n_col, be_n_col, beta_col)
#   if(any(!columns_to_check %in% colnames(data))) stop("Column not found in the data.")
#   
#   # Filter and summarize data
#   summary <- data %>%
#     filter(!!sym(fe_f_col) > 10, !!sym(be_f_col) > 10, !!sym(fe_n_col) > 10, !!sym(be_n_col) > 10) %>%
#     summarise(
#       count = n(),
#       median_stat = list(median_ci(!!sym(beta_col))),
#       mean_stat = list(mean_ci(!!sym(beta_col)))
#     ) %>%
#     mutate(
#       variable = beta_col,
#       mean = mean_stat[[1]]['mean'],
#       mean_ci_lower = mean_stat[[1]]['ci_lower'],
#       mean_ci_upper = mean_stat[[1]]['ci_upper'],
#       median = median_stat[[1]]['median'],
#       median_ci_lower = median_stat[[1]]['ci_lower'],
#       median_ci_upper = median_stat[[1]]['ci_upper']
#     ) %>%
#     select(-median_stat, -mean_stat)
#   
#   # Calculate p-values
#   beta_values <- na.omit(data[[beta_col]])
#   beta_values <- beta_values[is.finite(beta_values) & beta_values > 0 & beta_values != Inf & beta_values != -Inf]
#   
#   t_test_result <- t.test(beta_values, mu = 1, alternative = "greater", conf.int = TRUE)
#   wilcox_test_result <- wilcox.test(beta_values, mu = 1, alternative = "greater", conf.int = TRUE)
#   
#   # Add p-values and confidence intervals to the summary
#   summary <- summary %>%
#     mutate(
#       mean_p_value = t_test_result$p.value,
#       mean_ci_lower_ttest = t_test_result$conf.int[1],
#       mean_ci_upper_ttest = t_test_result$conf.int[2],
#       median_p_value = wilcox_test_result$p.value,
#       median_ci_lower_wilcox = wilcox_test_result$conf.int[1],
#       median_ci_upper_wilcox = wilcox_test_result$conf.int[2]
#     )
#   
#   return(summary)
# }

#===========================
# Create a function to apply summarize_stats to each set of conditions
apply_summary <- function(df, conditions) {
  map_dfr(conditions, ~summarize_stats(df, .x[1], .x[2], .x[3], .x[4], .x[5]))
}


# Define variable names for conditions
conditions <- list(
  c("wppooled_F",  "ppooled_F", "wppooled_Obs",  "ppooled_Obs", "b_wpp"),
  c("wpspooled_F", "ppooled_F", "wpspooled_Obs", "ppooled_Obs", "b_wpsp"),
  c("nppooled_F",  "ppooled_F", "nppooled_Obs",  "ppooled_Obs", "b_npp")
  # c("wpFE_F",  "pFE_E",  "wpFE_Obs",  "pFE_Obs", "b_wpp_FE"),
  # c("wpsFE_F", "pFE_E",  "wpsFE_Obs", "pFE_Obs", "b_wpsp_FE"),
  # c("npFE_F",  "pFE_E",  "npFE_Obs",  "pFE_Obs", "b_npp_FE"),
  # c( "wpBE_F",   "pBE_F",  "wpBE_Obs",  "pBE_Obs", "b_wp_BE"),
  # c( "wpsBE_F",  "pBE_F",  "wpsBE_Obs", "pBE_Obs", "b_wps_BE"),
  # c( "npBE_F",   "pBE_F",  "npBE_Obs",  "pBE_Obs", "b_np_BE")
)


# Remove inf values 
df_list$Percent1 <- df_list$Percent1 %>%
  mutate(
    b_wpp = ifelse(b_wpp == Inf, NA, b_wpp),
    b_wpsp = ifelse(b_wpsp == Inf, NA, b_wpsp),
    b_npp = ifelse(b_npp == Inf, NA, b_npp)
    
    # b_wpp_FE = ifelse(b_wpp_FE == Inf, NA, b_wpp_FE),
    # b_wpsp_FE = ifelse(b_wpsp_FE == Inf, NA, b_wpsp_FE),
    # b_npp_FE = ifelse(b_npp_FE == Inf, NA, b_npp_FE),
    # 
    # b_wpp_BE = ifelse(b_wpp_BE == Inf, NA, b_wpp_BE),
    # b_wpsp_BE = ifelse(b_wpsp_BE == Inf, NA, b_wpsp_BE),
    # b_npp_BE = ifelse(b_npp_BE == Inf, NA, b_npp_BE)
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
writeLines(latex_code, "MaivePhi2.tex")



# hist(df_list$Percent1[df_list$Percent1$beta<10,]$beta, breaks = 500)

# mean(df_list$Percent1[df_list$Percent1$allBE_F>10 & df_list$Percent1$allFE_F>10  & df_list$Percent1$beta_p > 0 & is.finite(df_list$Percent1$beta_p) & df_list$Percent1$pBE_Obs>10 & df_list$Percent1$pFE_Obs>10, ]$beta_p)


