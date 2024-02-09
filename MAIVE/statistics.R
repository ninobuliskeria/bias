library(plm)
library(dplyr)
library(weights)
library(ggplot2)
library(quantreg)
library("readxl")
# rm(list = ls())
setwd("H:/My Drive/BIAS/GitHub/maive") 
# Import data ####
OLS <- read_excel("MAIVE_allpooled_1.xlsx")  %>%filter(F>10)%>% select(Row_Names, SE) %>% rename(OLS = SE) 
FE  <-read_excel("MAIVE_allFE_1.xlsx")   %>%filter(F>10)%>% select(Row_Names,SE) %>% rename(FE = SE)
BE  <- read_excel("MAIVE_allBE_1.xlsx")   %>%filter(F>10)%>% select(Row_Names,SE)%>% rename(BE = SE)

all <- merge(OLS, FE, by="Row_Names", all.x=TRUE, all.y=TRUE)
all <- merge(all, BE, by="Row_Names", all.x=TRUE, all.y=TRUE)
results <- all %>% select(OLS,FE,BE ) 
results$psi <- abs(results$FE/results$BE)

#==============================================
pOLS <- read_excel("MAIVE_ppooled_1.xlsx")%>%filter(F>10)  %>% select(Row_Names,SE) %>% rename(pOLS = SE)
pFE  <-read_excel("MAIVE_pFE_1.xlsx") %>%filter(F>10) %>% select(Row_Names,SE) %>% rename(pFE = SE)
pBE  <- read_excel("MAIVE_pBE_1.xlsx") %>%filter(F>10)  %>% select(Row_Names,SE) %>% rename(pBE = SE)

P <- merge(pOLS, pFE, by="Row_Names", all.x=TRUE, all.y=TRUE)
P <- merge(P, pBE, by="Row_Names", all.x=TRUE, all.y=TRUE)
p_results <- P %>% select(pOLS,pFE,pBE ) 
p_results$psi <- abs(p_results$pFE/p_results$pBE)
#==============================================
wpOLS <- read_excel("MAIVE_wppooled_1.xlsx") %>%filter(F>10)  %>% select(Row_Names,SE) %>% rename(wpOLS = SE)
wpFE  <-read_excel("MAIVE_wpFE_1.xlsx")   %>%filter(F>10) %>% select(Row_Names,SE)%>% rename(wpFE = SE)
wpBE  <- read_excel("MAIVE_wpBE_1.xlsx")  %>%filter(F>10)  %>% select(Row_Names,SE) %>% rename(wpBE = SE)

WP <- merge(wpOLS, wpFE, by="Row_Names", all.x=TRUE, all.y=TRUE)
WP <- merge(WP, wpBE, by="Row_Names", all.x=TRUE, all.y=TRUE)
wp_results <- WP %>% select(wpOLS,wpFE,wpBE ) 
wp_results$psi <- abs(wp_results$wpFE/wp_results$wpBE)




# CONFIDENCE INTERVALS ####
index_estimates <-  p_results$psi
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