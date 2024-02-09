library(plm)
library(dplyr)
library(weights)
library(ggplot2)
library(quantreg)
library("readxl")
# rm(list = ls())
setwd("H:/My Drive/BIAS/GitHub/maive/maive-PETPEESE") 

#all==============================================
# Import data ####
OLS <- read_excel("MAIVE_allpooled_1.xlsx")  %>%filter(F>10)%>% select(Row_Names, E) %>% rename(OLS = E) 
FE  <- read_excel("MAIVE_allFE_1.xlsx")   %>%filter(F>10)%>% select(Row_Names,E) %>% rename(FE = E)
BE  <- read_excel("MAIVE_allBE_1.xlsx")   %>%filter(F>10)%>% select(Row_Names,E)%>% rename(BE = E)

all <- merge(OLS, FE, by="Row_Names", all.x=TRUE, all.y=TRUE)
all <- merge(all, BE, by="Row_Names", all.x=TRUE, all.y=TRUE)
results <- all %>% select("Row_Names", OLS,FE,BE ) 
results$psi <- abs(results$FE/results$BE)


# CONFIDENCE INTERVALS ####
index_estimates <-  results[results$psi < 1000,]$psi
index_estimates <- index_estimates[!is.na(index_estimates)]

# Confidence Interval for the Mean
mean_of_estimates <- mean(index_estimates, na.rm = TRUE)
sd_of_estimates <- sd(index_estimates)
n <- length(index_estimates)
error_margin <- qt(0.975, df=n-1) * sd_of_estimates/sqrt(n)
ci_mean <- c(mean_of_estimates - error_margin, mean_of_estimates + error_margin)

# Bootstrap Confidence Interval for the Median
bootstrap_samples <- 10000
medians <- numeric(bootstrap_samples)

for(i in 1:bootstrap_samples) {
  sample_data <- sample(index_estimates, size=n, replace=TRUE)
  medians[i] <- median(sample_data)
}

ci_median <- quantile(medians, c(0.025, 0.975))

mean(index_estimates, na.rm = TRUE)
cat("95% CI for the mean:", ci_mean, "\n")
median(index_estimates, na.rm = TRUE)
cat("95% CI for the median:", ci_median, "\n")
sum(!is.na(index_estimates))
 


#published papers==============================================
pOLS <- read_excel("MAIVE_ppooled_1.xlsx")%>%filter(F>10)  %>% select(Row_Names,E) %>% rename(pOLS = E)
pFE  <-read_excel("MAIVE_pFE_1.xlsx") %>%filter(F>10) %>% select(Row_Names,E) %>% rename(pFE = E)
pBE  <- read_excel("MAIVE_pBE_1.xlsx") %>%filter(F>10)  %>% select(Row_Names,E) %>% rename(pBE = E)

P <- merge(pOLS, pFE, by="Row_Names", all.x=TRUE, all.y=TRUE)
P <- merge(P, pBE, by="Row_Names", all.x=TRUE, all.y=TRUE)
p_results <- P %>% select("Row_Names", pOLS,pFE,pBE ) 
p_results$psi <- abs(p_results$pFE/p_results$pBE)

# CONFIDENCE INTERVALS ####
index_estimates <-  p_results[p_results$psi < 1000,]$psi
index_estimates <- index_estimates[!is.na(index_estimates)]

# Confidence Interval for the Mean
mean_of_estimates <- mean(index_estimates, na.rm = TRUE)
sd_of_estimates <- sd(index_estimates)
n <- length(index_estimates)
error_margin <- qt(0.975, df=n-1) * sd_of_estimates/sqrt(n)
ci_mean <- c(mean_of_estimates - error_margin, mean_of_estimates + error_margin)

# Bootstrap Confidence Interval for the Median
bootstrap_samples <- 10000
medians <- numeric(bootstrap_samples)

for(i in 1:bootstrap_samples) {
  sample_data <- sample(index_estimates, size=n, replace=TRUE)
  medians[i] <- median(sample_data)
}

ci_median <- quantile(medians, c(0.025, 0.975))

mean(index_estimates, na.rm = TRUE)
cat("95% CI for the mean:", ci_mean, "\n")
median(index_estimates, na.rm = TRUE)
cat("95% CI for the median:", ci_median, "\n")
sum(!is.na(index_estimates))
 
#working papers==============================================
wpOLS <- read_excel("MAIVE_wppooled_1.xlsx") %>%filter(F>10)  %>% select(Row_Names,E) %>% rename(wpOLS = E)
wpFE  <-read_excel("MAIVE_wpFE_1.xlsx")   %>%filter(F>10) %>% select(Row_Names,E)%>% rename(wpFE = E)
wpBE  <- read_excel("MAIVE_wpBE_1.xlsx")  %>%filter(F>10)  %>% select(Row_Names,E) %>% rename(wpBE = E)

WP <- merge(wpOLS, wpFE, by="Row_Names", all.x=TRUE, all.y=TRUE)
WP <- merge(WP, wpBE, by="Row_Names", all.x=TRUE, all.y=TRUE)
wp_results <- WP %>% select("Row_Names", wpOLS,wpFE,wpBE ) 
wp_results$psi <- abs(wp_results$wpFE/wp_results$wpBE)




# CONFIDENCE INTERVALS ####
index_estimates <-  wp_results[wp_results$psi<1000,]$psi
index_estimates <- index_estimates[!is.na(index_estimates)]

# Confidence Interval for the Mean
mean_of_estimates <- mean(index_estimates, na.rm = TRUE)
sd_of_estimates <- sd(index_estimates)
n <- length(index_estimates)
error_margin <- qt(0.975, df=n-1) * sd_of_estimates/sqrt(n)
ci_mean <- c(mean_of_estimates - error_margin, mean_of_estimates + error_margin)

# Bootstrap Confidence Interval for the Median
bootstrap_samples <- 10000
medians <- numeric(bootstrap_samples)

for(i in 1:bootstrap_samples) {
  sample_data <- sample(index_estimates, size=n, replace=TRUE)
  medians[i] <- median(sample_data)
}

ci_median <- quantile(medians, c(0.025, 0.975))

mean(index_estimates, na.rm = TRUE)
cat("95% CI for the mean:", ci_mean, "\n")
median(index_estimates, na.rm = TRUE)
cat("95% CI for the median:", ci_median, "\n")
sum(!is.na(index_estimates))
 


#published papers vs working papers ==============================================
out <- merge(results, p_results, by="Row_Names", all.x=TRUE, all.y=TRUE)
out <- merge(out, wp_results, by="Row_Names", all.x=TRUE, all.y=TRUE)

wppp <- out %>% 
  select("Row_Names", "pOLS", "pFE", "pBE", "wpOLS", "wpFE", "wpBE")%>% 
  mutate(psiOLS = abs(wpOLS/pOLS), 
         psiFE  = abs(wpFE/pFE),
         psiBE  = abs(wpBE/pBE)
            )
# CONFIDENCE INTERVALS ####
#select
#OLS ==============================================
index_estimates <-  wppp[wppp$psiOLS<1000,]$psiOLS
index_estimates <- index_estimates[!is.na(index_estimates)]

# Confidence Interval for the Mean
mean_of_estimates <- mean(index_estimates, na.rm = TRUE)
sd_of_estimates <- sd(index_estimates)
n <- length(index_estimates)
error_margin <- qt(0.975, df=n-1) * sd_of_estimates/sqrt(n)
ci_mean <- c(mean_of_estimates - error_margin, mean_of_estimates + error_margin)

# Bootstrap Confidence Interval for the Median
bootstrap_samples <- 10000
medians <- numeric(bootstrap_samples)

for(i in 1:bootstrap_samples) {
  sample_data <- sample(index_estimates, size=n, replace=TRUE)
  medians[i] <- median(sample_data)
}

ci_median <- quantile(medians, c(0.025, 0.975))

mean(index_estimates, na.rm = TRUE)
cat("95% CI for the mean:", ci_mean, "\n")
median(index_estimates, na.rm = TRUE)
cat("95% CI for the median:", ci_median, "\n")
sum(!is.na(index_estimates))

 
#FE ==============================================
index_estimates <-  wppp[wppp$psiFE<1000,]$psiFE
index_estimates <- index_estimates[!is.na(index_estimates)]

# Confidence Interval for the Mean
mean_of_estimates <- mean(index_estimates, na.rm = TRUE)
sd_of_estimates <- sd(index_estimates)
n <- length(index_estimates)
error_margin <- qt(0.975, df=n-1) * sd_of_estimates/sqrt(n)
ci_mean <- c(mean_of_estimates - error_margin, mean_of_estimates + error_margin)

# Bootstrap Confidence Interval for the Median
bootstrap_samples <- 10000
medians <- numeric(bootstrap_samples)

for(i in 1:bootstrap_samples) {
  sample_data <- sample(index_estimates, size=n, replace=TRUE)
  medians[i] <- median(sample_data)
}
ci_median <- quantile(medians, c(0.025, 0.975))

mean(index_estimates, na.rm = TRUE)
cat("95% CI for the mean:", ci_mean, "\n")
median(index_estimates, na.rm = TRUE)
cat("95% CI for the median:", ci_median, "\n")
sum(!is.na(index_estimates))
 
#BE ==============================================
index_estimates <-  wppp[wppp$psiBE<1000,]$psiBE
index_estimates <- index_estimates[!is.na(index_estimates)]

# Confidence Interval for the Mean
mean_of_estimates <- mean(index_estimates, na.rm = TRUE)
sd_of_estimates <- sd(index_estimates)
n <- length(index_estimates)
error_margin <- qt(0.975, df=n-1) * sd_of_estimates/sqrt(n)
ci_mean <- c(mean_of_estimates - error_margin, mean_of_estimates + error_margin)

# Bootstrap Confidence Interval for the Median
bootstrap_samples <- 10000
medians <- numeric(bootstrap_samples)

for(i in 1:bootstrap_samples) {
  sample_data <- sample(index_estimates, size=n, replace=TRUE)
  medians[i] <- median(sample_data)
}

ci_median <- quantile(medians, c(0.025, 0.975))

mean(index_estimates, na.rm = TRUE)
cat("95% CI for the mean:", ci_mean, "\n")
median(index_estimates, na.rm = TRUE)
cat("95% CI for the median:", ci_median, "\n")
sum(!is.na(index_estimates))
 
