library(tidyverse)
library(writexl)
library(readxl)
library(boot)
library(xtable)

library("tidyverse") # This will load ggplot2, dplyr, readxl, and other essential packages
library("writexl")
library("readxl")

rm(list = ls())
# Set Working Directory
setwd("H:/My Drive/BIAS/bias/MAIVE")

# Import Data ####
allFE  <- read_excel("MAIVE_allFE.xlsx") 
pFE    <- read_excel("MAIVE_pFE.xlsx") 
wpFE   <- read_excel("MAIVE_wpFE.xlsx") 
wpsFE  <- read_excel("MAIVE_wpsFE.xlsx") 
npFE   <- read_excel("MAIVE_npFE.xlsx") 

allBE  <- read_excel("MAIVE_allBE.xlsx") 
pBE    <- read_excel("MAIVE_pBE.xlsx") 
wpBE   <- read_excel("MAIVE_wpBE.xlsx") 
wpsBE  <- read_excel("MAIVE_wpsBE.xlsx") 
npBE   <- read_excel("MAIVE_npBE.xlsx") 

# Rename Variables ####
allFE <- allFE %>% rename_with(~c("E_FE", "SE_FE", "F_FE", "Hausman_FE", "CV_FE", "Obs_FE"),
                               .cols = c("E", "SE", "F", "Hausman", "CV_of_Chi2", "Obs"))
pFE   <- pFE %>% rename_with(~c("E_pFE", "SE_pFE", "F_pFE", "Hausman_pFE", "CV_pFE", "Obs_pFE"),
                             .cols = c("pE", "pSE", "pF", "pHausman", "pCV", "pObs"))
wpFE  <- wpFE %>% rename_with(~c("E_wpFE", "SE_wpFE", "F_wpFE", "Hausman_wpFE", "CV_wpFE", "Obs_wpFE"),
                              .cols = c("wpE", "wpSE", "wpF", "wpHausman", "wpCV", "wpObs"))
wpsFE <- wpsFE %>% rename_with(~c("E_wpsFE", "SE_wpsFE", "F_wpsFE", "Hausman_wpsFE", "CV_wpsFE", "Obs_wpsFE"),
                               .cols = c("wpsE", "wpsSE", "wpsF", "wpsHausman", "wpsCV", "wpsObs"))
npFE  <- npFE %>% rename_with(~c("E_npFE", "SE_npFE", "F_npFE", "Hausman_npFE", "CV_npFE", "Obs_npFE"),
                              .cols = c("npE", "npSE", "npF", "npHausman", "npCV", "npObs"))

allBE    <- allBE %>% rename_with(~c("E_BE", "SE_BE", "F_BE", "Hausman_BE", "CV_BE", "Obs_BE"),
                                  .cols = c("E", "SE", "F", "Hausman", "CV_of_Chi2", "Obs"))
pBE      <- pBE %>% rename_with(~c("E_pBE", "SE_pBE", "F_pBE", "Hausman_pBE", "CV_pBE", "Obs_pBE"),
                                .cols = c("pE", "pSE", "pF", "pHausman", "pCV", "pObs"))
wpBE     <- wpBE %>% rename_with(~c("E_wpBE", "SE_wpBE", "F_wpBE", "Hausman_wpBE", "CV_wpBE", "Obs_wpBE"),
                                 .cols = c("wpE", "wpSE", "wpF", "wpHausman", "wpCV", "wpObs"))
wpsBE    <- wpsBE %>% rename_with(~c("E_wpsBE", "SE_wpsBE", "F_wpsBE", "Hausman_wpsBE", "CV_wpsBE", "Obs_wpsBE"),
                                  .cols = c("wpsE", "wpsSE", "wpsF", "wpsHausman", "wpsCV", "wpsObs"))
npBE     <- npBE %>% rename_with(~c("E_npBE", "SE_npBE", "F_npBE", "Hausman_npBE", "CV_npBE", "Obs_npBE"),
                                 .cols = c("npE", "npSE", "npF", "npHausman", "npCV", "npObs"))

# Merge Data ####
data <- list(allFE, pFE, wpFE, wpsFE, npFE, allBE, pBE, wpBE, wpsBE, npBE) %>%
  reduce(full_join, by = "Row_Names")

create_beta_columns <- function(df) {
  mutate(df,
         beta = if_else(!is.infinite(E_FE / E_BE) & !is.na(E_FE / E_BE), abs(E_FE / E_BE), NA_real_),
         beta_p = if_else(!is.infinite(E_pFE / E_pBE) & !is.na(E_pFE / E_pBE), abs(E_pFE / E_pBE), NA_real_),
         beta_wp = if_else(!is.infinite(E_wpFE / E_wpBE) & !is.na(E_wpFE / E_wpBE), abs(E_wpFE / E_wpBE), NA_real_),
         beta_wps = if_else(!is.infinite(E_wpsFE / E_wpsBE) & !is.na(E_wpsFE / E_wpsBE), abs(E_wpsFE / E_wpsBE), NA_real_),
         beta_np = if_else(!is.infinite(E_npFE / E_npBE) & !is.na(E_npFE / E_npBE), abs(E_npFE / E_npBE), NA_real_)
  )
}

data_with_beta <- create_beta_columns(data)

mean_ci <- function(x) {
  x <- na.omit(x)
  n <- length(x)
  se <- sd(x) / sqrt(n)
  mean_x <- mean(x)
  ci <- qnorm(c(0.025, 0.975), mean = mean_x, sd = se)
  c(mean = mean_x, ci_lower = ci[1], ci_upper = ci[2])
}

median_ci <- function(x, n = 1000) {
  x <- na.omit(x)
  if (length(x) < 2) return(c(median = NA, ci_lower = NA, ci_upper = NA))
  boot_obj <- boot(x, statistic = function(x, i) median(x[i]), R = n)
  ci <- boot.ci(boot_obj, conf = 0.95, type = "perc")$percent[4:5]
  c(median = median(x), ci_lower = ci[1], ci_upper = ci[2])
}

summarize_stats <- function(data, fe_f_test, be_f_test, fe_n_col, be_n_col, beta_col) {
  # Filter and summarize data
  summary <- data %>%
    filter(!is.na(!!sym(fe_f_test)) & !is.na(!!sym(be_f_test)),
           !!sym(fe_f_test) > 10, !!sym(be_f_test) > 10,
           !is.na(!!sym(fe_n_col)) & !is.na(!!sym(be_n_col)),
           !!sym(fe_n_col) > 10, !!sym(be_n_col) > 10) %>%
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

apply_summary <- function(df, conditions) {
  map_dfr(conditions, ~summarize_stats(df, .x[1], .x[2], .x[3], .x[4], .x[5]))
}

conditions <- list(
  c("F_FE", "F_BE", "Obs_FE", "Obs_BE", "beta"),
  c("F_pFE", "F_pBE", "Obs_pFE", "Obs_pBE", "beta_p"),
  c("F_wpFE", "F_wpBE", "Obs_wpFE", "Obs_wpBE", "beta_wp"),
  c("F_wpsFE", "F_wpsBE", "Obs_wpsFE", "Obs_wpsBE", "beta_wps"),
  c("F_npFE", "F_npBE", "Obs_npFE", "Obs_npBE", "beta_np")
)

final_results <- apply_summary(data_with_beta, conditions)

print(final_results)

xtable_object <- xtable(final_results)
print(xtable_object, include.rownames = FALSE, floating = FALSE)
latex_code <- capture.output(print(xtable_object, include.rownames = FALSE, floating = FALSE))
writeLines(latex_code, "MaivePhi.tex")
