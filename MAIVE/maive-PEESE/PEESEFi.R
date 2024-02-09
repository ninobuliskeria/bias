library("tidyverse") # This will load ggplot2, dplyr, readxl, and other essential packages
library("writexl")
library("boot")
library("readxl")

rm(list = ls())
# Set Working Directory
setwd("H:/My Drive/BIAS/bias/PEES/MAIVE")
 
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
      beta      = abs(allFE_E/allBE_E),
      beta_p    = abs(pFE_E/pBE_E),
      beta_wp   = abs(wpFE_E/wpBE_E),
      beta_wps  = abs(wpsFE_E/wpsBE_E),
      beta_np   = abs(npFE_E/npBE_E)
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
  c("allFE_F", "allBE_F", "allFE_Obs", "allBE_Obs", "beta"),
  c(  "pFE_F",   "pBE_F",   "pFE_Obs",   "pBE_Obs", "beta_p"),
  c( "wpFE_F",  "wpBE_F",  "wpFE_Obs",  "wpBE_Obs", "beta_wp"),
  c("wpsFE_F", "wpsBE_F", "wpsFE_Obs", "wpsBE_Obs", "beta_wps"),
  c( "npFE_F",  "npBE_F",  "npFE_Obs",  "npBE_Obs", "beta_np")
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
writeLines(latex_code, "PEESEPhi.tex")





# mean(df_list$Percent1[df_list$Percent1$allBE_F>10 & df_list$Percent1$allFE_F>10  & df_list$Percent1$beta_p > 0 & is.finite(df_list$Percent1$beta_p) & df_list$Percent1$pBE_Obs>10 & df_list$Percent1$pFE_Obs>10, ]$beta_p)



















































# 
# 
# 
# 
# 
# merged_df <- Reduce(function(x, y) merge(x, y, by = "Row_Names", all = TRUE), list_of_dataframes)
# beta <- merged_df %>% select("Row_Names",              
#                              "allBE_1_E",      "allBE_2_E",      "allBE_5_E",
#                              "allFE_1_E",      "allFE_2_E",      "allFE_5_E",
#                              "allpooled_1_E",  "allpooled_2_E",  "allpooled_5_E",  
#                              
#                              "npBE_1_E",      "npBE_2_E",       "npBE_5_E",  
#                              "npFE_1_E",      "npFE_2_E",         "npFE_5_E",        
#                              "nppooled_1_E",  "nppooled_2_E",   "nppooled_5_E",
#                              
#                              "pBE_1_E",      "pBE_2_E", "pBE_5_E",
#                              "pFE_2_E",       "pFE_2_E", "pFE_5_E",
#                              "ppooled_1_E",   "ppooled_2_E","ppooled_5_E",
#                              
#                              "wpBE_1_E",      "wpBE_2_E",         "wpBE_5_E",
#                              "wpFE_1_E",      "wpFE_2_E",         "wpFE_5_E", 
#                              "wppooled_1_E",  "wppooled_2_E",     "wppooled_5_E",       
#                              
#                              "wpsBE_1_E",     "wpsBE_2_E","wpsBE_5_E",
#                              "wpsFE_1_E",     "wpsFE_2_E", "wpsFE_5_E", 
#                              "wpspooled_1_E", "wpspooled_2_E", "wpspooled_5_E"  ) 
# colnames(beta) <- gsub("_E$", "", colnames(beta))
# colnames(beta) 
# # Define a function to create beta columns
# create_beta_columns <- function(df) {
#   mutate(df,
#          beta_1 = abs(allFE_1 / allBE_1),
#          beta_1 = abs(allFE_1 / allBE_1),
#          beta_1 = abs(allFE_1 / allBE_1),
#          
#          beta_p_2 = abs(pFE_2 / pBE_2),
#          
#          beta_wp = abs(E_wpFE / E_wpBE),
#          beta_wps = abs(E_wpsFE / E_wpsBE),
#          beta_np = abs(E_npFE / E_npBE)
#   )
# }
# 
# # Import Data ####
# allFE  <- read_excel("MAIVE_allFE.xlsx") 
# pFE    <- read_excel("MAIVE_pFE.xlsx") 
# wpFE   <- read_excel("MAIVE_wpFE.xlsx") 
# wpsFE  <- read_excel("MAIVE_wpsFE.xlsx") 
# npFE   <- read_excel("MAIVE_npFE.xlsx") 
# 
# allBE  <- read_excel("MAIVE_allBE.xlsx") 
# pBE    <- read_excel("MAIVE_pBE.xlsx") 
# wpBE   <- read_excel("MAIVE_wpBE.xlsx") 
# wpsBE  <- read_excel("MAIVE_wpsBE.xlsx") 
# npBE   <- read_excel("MAIVE_npBE.xlsx") 
# 
# 
# 
# # Rename Variables ####
# allFE <- allFE %>% rename_with(~c("E_FE", "SE_FE", "F_FE", "Hausman_FE", "CV_FE", "Obs_FE"),
#                          .cols = c("E", "SE", "F", "Hausman", "CV_of_Chi2", "Obs"))
# pFE   <- pFE %>% rename_with(~c("E_pFE", "SE_pFE", "F_pFE", "Hausman_pFE", "CV_pFE", "Obs_pFE"),
#                              .cols = c("pE", "pSE", "pF", "pHausman", "pCV", "pObs"))
# wpFE  <- wpFE %>% rename_with(~c("E_wpFE", "SE_wpFE", "F_wpFE", "Hausman_wpFE", "CV_wpFE", "Obs_wpFE"),
#                               .cols = c("wpE", "wpSE", "wpF", "wpHausman", "wpCV", "wpObs"))
# wpsFE <- wpsFE %>% rename_with(~c("E_wpsFE", "SE_wpsFE", "F_wpsFE", "Hausman_wpsFE", "CV_wpsFE", "Obs_wpsFE"),
#                                .cols = c("wpsE", "wpsSE", "wpsF", "wpsHausman", "wpsCV", "wpsObs"))
# npFE  <- npFE %>% rename_with(~c("E_npFE", "SE_npFE", "F_npFE", "Hausman_npFE", "CV_npFE", "Obs_npFE"),
#                               .cols = c("npE", "npSE", "npF", "npHausman", "npCV", "npObs"))
# 
# allBE    <- allBE %>% rename_with(~c("E_BE", "SE_BE", "F_BE", "Hausman_BE", "CV_BE", "Obs_BE"),
#                             .cols = c("E", "SE", "F", "Hausman", "CV_of_Chi2", "Obs"))
# pBE      <- pBE %>% rename_with(~c("E_pBE", "SE_pBE", "F_pBE", "Hausman_pBE", "CV_pBE", "Obs_pBE"),
#                             .cols = c("pE", "pSE", "pF", "pHausman", "pCV", "pObs"))
# wpBE     <- wpBE %>% rename_with(~c("E_wpBE", "SE_wpBE", "F_wpBE", "Hausman_wpBE", "CV_wpBE", "Obs_wpBE"),
#                             .cols = c("wpE", "wpSE", "wpF", "wpHausman", "wpCV", "wpObs"))
# wpsBE    <- wpsBE %>% rename_with(~c("E_wpsBE", "SE_wpsBE", "F_wpsBE", "Hausman_wpsBE", "CV_wpsBE", "Obs_wpsBE"),
#                             .cols = c("wpsE", "wpsSE", "wpsF", "wpsHausman", "wpsCV", "wpsObs"))
# npBE     <- npBE %>% rename_with(~c("E_npBE", "SE_npBE", "F_npBE", "Hausman_npBE", "CV_npBE", "Obs_npBE"),
#                             .cols = c("npE", "npSE", "npF", "npHausman", "npCV", "npObs"))
# 
# # Merge Data ####
# data <- list(allFE, pFE, wpFE, wpsFE, npFE, allBE, pBE, wpBE, wpsBE, npBE) %>%
#   reduce(full_join, by = "Row_Names")
# 
# 
# # Define a function to create beta columns
# create_beta_columns <- function(df) {
#   mutate(df,
#          beta = abs(E_FE / E_BE),
#          beta_p = abs(E_pFE / E_pBE),
#          beta_wp = abs(E_wpFE / E_wpBE),
#          beta_wps = abs(E_wpsFE / E_wpsBE),
#          beta_np = abs(E_npFE / E_npBE)
#   )
# }
# 
# # Apply the function to the data frame
# data_with_beta <- create_beta_columns(data)
# 
# # Function to calculate the mean and its 95% CI
#  mean_ci <- function(x) {
#    se <- sd(x, na.rm = TRUE) / sqrt(length(x))
#    mean_x <- mean(x, na.rm = TRUE)
#    ci <- qnorm(c(0.025, 0.975), mean = mean_x, sd = se)
#    c(mean = mean_x, ci_lower = ci[1], ci_upper = ci[2])
#  }
#  
# # Function to calculate the median and its 95% CI using bootstrapping
#  median_ci <- function(x, n = 1000) {
#    boot_obj <- boot(x, statistic = function(x, i) median(x[i]), R = n)
#    ci <- boot.ci(boot_obj,conf = 0.95, type = "perc")$percent[4:5]
#    c(median = median(x, na.rm = TRUE), ci_lower = ci[1], ci_upper = ci[2])
#  }
#    
# # Define a custom function to summarize the data with CIs and p-values
#  summarize_stats <- function(data, fe_f_test, be_f_test, fe_n_col, be_n_col, beta_col) {
#    # Filter and summarize data
#    summary <- data %>%
#      filter(!!sym(fe_f_test) > 10, !!sym(be_f_test) > 10, !!sym(fe_n_col) > 10, !!sym(be_n_col) > 10) %>%
#      summarise(
#        count = n(),
#        median_stat = list(median_ci(!!sym(beta_col))),
#        mean_stat = list(mean_ci(!!sym(beta_col)))
#      ) %>%
#      mutate(
#        variable = beta_col,
#        mean = mean_stat[[1]]['mean'],
#        mean_ci_lower = mean_stat[[1]]['ci_lower'],
#        mean_ci_upper = mean_stat[[1]]['ci_upper'],
#        median = median_stat[[1]]['median'],
#        median_ci_lower = median_stat[[1]]['ci_lower'],
#        median_ci_upper = median_stat[[1]]['ci_upper']
#      ) %>%
#      select(-median_stat, -mean_stat)
#    
#    # Calculate p-values
#    beta_values <- na.omit(data[[beta_col]])
#    t_test_result <- t.test(beta_values, mu = 1, alternative = "greater")
#    wilcox_test_result <- wilcox.test(beta_values, mu = 1, alternative = "greater", conf.int = TRUE)
#    
#    # Add p-values to the summary
#    summary <- summary %>%
#      mutate(
#        mean_p_value = t_test_result$p.value,
#        median_p_value = wilcox_test_result$p.value
#      )
#    
#    return(summary)
#  }
#  
# # Create a function to apply summarize_stats to each set of conditions
# apply_summary <- function(df, conditions) {
#   map_dfr(conditions, ~summarize_stats(df, .x[1], .x[2], .x[3]))
# }
# 
# # Define variable names for conditions
# conditions <- list(
#   c("F_FE","F_BE", "Obs_FE",    "Obs_BE", "beta"),
#   c("F_pFE","F_BE", "Obs_pFE",   "Obs_pBE", "beta_p"),
#   c("F_wpFE","F_BE", "Obs_wpFE",  "Obs_wpBE", "beta_wp"),
#   c("F_wpsFE","F_BE", "Obs_wpsFE", "Obs_wpsBE", "beta_wps"),
#   c("F_npFE","F_BE", "Obs_npFE",  "Obs_npBE", "beta_np")
# )
# 
# # Apply the summarize_stats function to your data frame with the specified conditions
# final_results <- apply_summary(data_with_beta, conditions)
# final_results <- map_dfr(conditions, ~summarize_stats(data_with_beta, .x[1], .x[2], .x[3]))
# # View the results
# print(final_results)
# 
# # Continue with LaTeX code generation as before
# library(xtable)
# xtable_object <- xtable(final_results)
# print(xtable_object, include.rownames = FALSE, floating = FALSE)
# latex_code <- capture.output(print(xtable_object, include.rownames = FALSE, floating = FALSE))
# writeLines(latex_code, "MaivePhi.tex")
#    
#    
# 
# 
# data$b <- abs(data$E_FE/data$E_BE)
# 
# b <- data[is.finite(data$b) & data$F_FE>10 & data$F_BE>10 & data$Obs_BE > 10 & data$Obs_FE > 10 ,]$b
# 
# mean(b, na.rm = TRUE)
# median(b, na.rm = TRUE)
# 
# # Save Merged Results
# write_xlsx(merged_results, "MAIVEresults.xlsx")
# 
# # Cleanup
# rm(list = setdiff(ls(), "merged_results"))
#  