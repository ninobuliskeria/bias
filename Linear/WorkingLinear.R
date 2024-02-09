library(plm)
library(dplyr)
library(weights)
library(ggplot2)
library("readxl")
# rm(list = ls())
#setwd("H:/My Drive/BIAS/GitHub/Linear") 
# Import data ####
data  <- read_excel("H:/My Drive/BIAS/DATA/DataWorkReg.xlsx")  

dfreg_wp <- data[data$studyPublishD==0,] #only working papers 
# Define levels and initialize a list for matrices
# winsor_levels <- c(1, 2.5, 5)

# OLS #### 
OLS_matrices <- list()
# Initialize a matrix for this level
OLS_matrix <- matrix(NA, nrow = 613, ncol = 4)  # Assuming 2 coefficients (Intercept, SE1) and 2 standard errors

for (i in 1:613) {
  current_data <- dfreg_wp[dfreg_wp$metaID == i, ]
  
  if (nrow(current_data) > 0 && !all(is.na(current_data$E1)) && !all(is.na(current_data$SE1))) {
    result <- try({
      temp_result <- lm(E1 ~ SE1, data = current_data)
      
      coefs <- coef(temp_result)
      std_errors <- summary(temp_result)$coefficients[, "Std. Error"]
      
      # Ensure result has a consistent length, fill with NA if needed
      result_vector <- rep(NA, 4)
      result_vector[1:length(coefs)] <- coefs
      result_vector[(length(coefs) + 1):4] <- std_errors
      
      result_vector
    }, silent = TRUE)
    
    if (!inherits(result, "try-error") && length(result) == 4) {
      OLS_matrix[i, ] <- result
    }
  }
}


# Assign the matrix to the list
name <- "DATA"
OLS_matrices[[as.character(name)]] <- OLS_matrix

# Output matrices
OLS_matrices <- as.data.frame(OLS_matrices$DATA) 
OLS_matrices$OLS <- abs(OLS_matrices$V2)
resultsWP <- OLS_matrices %>% select("OLS")

# Fixed Effects #### 
FE_matrices <- list()
# Initialize a matrix for this level
FE_matrix <- matrix(NA, nrow = 613, ncol = 3) # Assuming `n` is defined somewhere

for (i in 1:613) {
  # Perform fixed effects regression
  result <- try({
    temp_result <- plm(E1 ~ SE1, data = dfreg_wp[dfreg_wp$metaID == i, ], 
                       model = "within", index = c("studyID"))
    temp_summary <- summary(temp_result, vcov = vcovHC(temp_result, method = "arellano"))
    standard_errors <- temp_summary$coefficients[, "Std. Error"]
    FE_matrix[i, ] <- c(i, coef(temp_result), standard_errors) # Fill in with appropriate values
  }, silent = TRUE)
  
  if (!inherits(result, "try-error")) {
    # print(i)
  } else {
    # Handle or log the error, then skip to the next iteration
    # print(paste("Error in iteration", i))
  }
}


# Assign the matrix to the list 
name <- "DATA"
FE_matrices[[as.character(name)]] <- FE_matrix

# Output matrices
FE_matrices <- as.data.frame(FE_matrices$DATA) 
FE_matrices$FE <- abs(FE_matrices$V2)
resultsWP$FE <- FE_matrices$FE 

# Between Effects #### 
avgen_wp <- dfreg_wp %>%
  group_by(metaID, studyID) %>%
  summarise(
    E1  = mean(E1, na.rm = TRUE),
    SE1 = mean(SE1, na.rm = TRUE),
    E2  = mean(E2, na.rm = TRUE),
    SE2 = mean(SE2, na.rm = TRUE),
    E5  = mean(E5, na.rm = TRUE),
    SE5 = mean(SE5, na.rm = TRUE),
    
    studyPublishD = mean(studyPublishD, na.rm = TRUE),
    np  = mean(np, na.rm = TRUE)
  )
BE_matrices <- list()
# Initialize a matrix for this level
BE_matrix <- matrix(NA, nrow = 613, ncol = 3) # Assuming `n` is defined somewhere

for (i in 1:613) {
  # Perform fixed effects regression within try block
  result <- try({
    temp_result <- plm(E1 ~ SE1, data = avgen_wp[avgen_wp$metaID == i, ])
    temp_summary <- summary(temp_result, vcov = vcovHC(temp_result, method = "arellano"))
    standard_errors <- temp_summary$coefficients[, "Std. Error"]
    BE_matrix[i, ] <- c(i, coef(temp_result), standard_errors) # Fill in with appropriate values
  }, silent = TRUE)
  
  if (!inherits(result, "try-error")) {
    # print(i)
  } else {
    # Handle or log the error, then skip to the next iteration
    # print(paste("Error in iteration", i))
  }
}


# Assign the matrix to the list 
name <- "DATA"
BE_matrices[[as.character(name)]] <- BE_matrix

# Output matrices
BE_matrices <- as.data.frame(BE_matrices$DATA) 
BE_matrices$BE <- abs(BE_matrices$V2)
resultsWP$BE <- BE_matrices$BE

# RESULTS ####
resultsWP$psi = abs(resultsWP$FE/resultsWP$BE)


# CONFIDENCE INTERVALS ####
resultsWP <- resultsWP[resultsWP$psi<1000,]
index_estimates <-  resultsWP$psi
index_estimates <- index_estimates[!is.na(index_estimates)]

# Confidence Interval for the Mean
mean_of_estimates <- mean(index_estimates, na.rm = TRUE)
sd_of_estimates <- sd(index_estimates, na.rm = TRUE)
n <- length(index_estimates)
error_margin <- qt(0.975, df=n-1) * sd_of_estimates/sqrt(n)
ci_mean_WP <- c(mean_of_estimates - error_margin, mean_of_estimates + error_margin)

# Bootstrap Confidence Interval for the Median
bootstrap_samples <- 10000
medians <- numeric(bootstrap_samples)
 
for(i in 1:bootstrap_samples) {
  sample_data <- sample(index_estimates, size=n, replace=TRUE)
  medians[i] <- median(sample_data)
}
ci_median_WP <- quantile(medians, c(0.025, 0.975))

# List all objects in the environment
all_objects <- ls()

# Remove all objects except for 'resultsWP' and 'resultsPP'
rm(list = all_objects[!all_objects %in% c( "data", "dfreg", "avgen", "share",
                                           "results",   "ci_mean",         "ci_median",
                                           "resultsPP", "ci_mean_PP",      "ci_median_PP",
                                           "resultsWP", "ci_mean_WP",      "ci_median_WP",
                                           "wppp",      "ci_mean_wppp",    "ci_median_wppp",
                                           "wppp_FE",   "ci_mean_wppp_FE", "ci_median_wppp_FE",
                                           "wppp_BE",   "ci_mean_wppp_BE", "ci_median_wppp_BE")])



# RESULTS ####
# Print the results
mean(resultsWP$psi, na.rm = TRUE)
cat("95% CI for the mean:", ci_mean_WP, "\n")
median(resultsWP$psi, na.rm = TRUE)
cat("95% CI for the median:", ci_median_WP, "\n")
sum(!is.na(resultsWP$psi))



#HISTOGRAM ####
ggplot() +
  geom_histogram(data = resultsWP, aes(x = psi, y = ..density.., color = "\u03C8=\u03B2(FE)/\u03B2(BE)"), 
                 binwidth = .25, linetype = "dashed", fill = "gray70", na.rm = TRUE, linewidth = 0.2) +
  scale_color_manual(values = c("\u03C8=\u03B2(FE)/\u03B2(BE)" = "black")) +
  scale_fill_manual(values = c("\u03C8=\u03B2(FE)/\u03B2(BE)" = "gray70"),
                    guide = guide_legend(override.aes = list(fill = c("gray70", NA, NA)))) +
  labs(x = "", y = "", color = " ", fill = " ") +
  geom_vline(xintercept = 1, color = "red", linetype = "dashed", linewidth = 0.2) +
  theme_minimal() +
  theme(legend.box.background = element_blank(),  
        legend.box.margin = margin(-15,3, 3, 3, unit = "pt"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(.8, .6)) +
  scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 1))  



ggplot() +
  geom_histogram(data = resultsWP, aes(x = OLS, y = ..density.., color = "Selection Bias (OLS)"), 
                 binwidth = .25, linetype = "dashed", fill = "gray70", na.rm = TRUE, linewidth = 0.2) +
  geom_histogram(data = resultsPP, aes(x = BE, y = ..density.., color = "Publication Bias (BE)"), 
                 binwidth = .25, linetype = "dashed", fill = NA, na.rm = TRUE, linewidth = .7) +
  geom_histogram(data = resultsPP, aes(x = FE, y = ..density.., color = "p-Hacking (FE)"), 
                 binwidth = .25, linetype = "dashed", fill = NA, na.rm = TRUE, linewidth = .7) +
  scale_color_manual(values = c("Selection Bias (OLS)" = "gray70", "Publication Bias (BE)" = "red", "p-Hacking (FE)" = "darkgreen")) +
  scale_fill_manual(values = c("Selection Bias (OLS)" = "gray70", "Publication Bias (BE)" = NA, "p-Hacking (FE)" = NA),
                    guide = guide_legend(override.aes = list(fill = c("gray70", NA, NA)))) +
  labs(x = "", y = "", color = " ", fill = " ") +
  theme_minimal() +
  theme(legend.box.background = element_blank(),  
        legend.box.margin = margin(-15,3, 3, 3, unit = "pt"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(.8, .6)) + 
  scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 1))  
  

