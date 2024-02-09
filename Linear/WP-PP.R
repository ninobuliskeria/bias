# Assuming df is your dataframe 
# resultsWP$Index <- seq.int(nrow(resultsWP)) 
# resultsWP <- rbind(resultsWP, rep(NA, ncol(resultsWP))) 
# resultsWP <- resultsWP %>% rename(wpOLS = OLS, wpFE =  FE,  wpBE=  BE,  wpPSI =  psi)
# resultsPP$Index <- seq.int(nrow(resultsPP))
# resultsPP <- resultsPP%>% rename(ppOLS = OLS, ppFE =  FE,  ppBE=  BE,  ppPSI =  psi)
# results$Index <- seq.int(nrow(results))

df_wppp <- merge(results, resultsPP, by="Index", all.x=TRUE, all.y=TRUE)
df_wppp <- merge(df_wppp, resultsWP, by="Index", all.x=TRUE, all.y=TRUE)

df_wppp$wppp <- abs(df_wppp$wpOLS/df_wppp$ppOLS)
#HISTOGRAM ####
#HISTOGRAM ####
ggplot() +
  geom_histogram(data = df_wppp, aes(x = wppp, y = ..density.., color = "\u03C8=\u03B2(wp)/\u03B2(pp)"), 
                 binwidth = .25, linetype = "dashed", fill = "gray70", na.rm = TRUE, linewidth = 0.2) +
  scale_color_manual(values = c("\u03C8=\u03B2(wp)/\u03B2(pp)" = "black")) +
  scale_fill_manual(values = c("\u03C8=\u03B2(wp)/\u03B2(pp)" = "gray70"),
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

 
#===================================================================================
wppp<-abs(df_wppp$wpOLS/df_wppp$ppOLS)
wppp <- wppp[wppp<1000]
# CONFIDENCE INTERVALS ####
index_estimates <-  wppp
index_estimates <- index_estimates[!is.na(index_estimates)]

# Confidence Interval for the Mean
mean_of_estimates <- mean(index_estimates, na.rm = TRUE)
sd_of_estimates <- sd(index_estimates)
n <- length(index_estimates)
error_margin <- qt(0.975, df=n-1) * sd_of_estimates/sqrt(n)
ci_mean_wppp <- c(mean_of_estimates - error_margin, mean_of_estimates + error_margin)

# Bootstrap Confidence Interval for the Median
bootstrap_samples <- 10000
medians <- numeric(bootstrap_samples)

for(i in 1:bootstrap_samples) {
  sample_data <- sample(index_estimates, size=n, replace=TRUE)
  medians[i] <- median(sample_data)
}

ci_median_wppp <- quantile(medians, c(0.025, 0.975))


# RESULTS ####
# WP/PP
mean(wppp, na.rm = TRUE)
cat("95% CI for the mean:", ci_mean_wppp, "\n")
median(wppp, na.rm = TRUE)
cat("95% CI for the median:", ci_median_wppp, "\n")
sum(!is.na(wppp))




#Fixed Effects ####
wppp_FE<-abs(df_wppp$wpFE/df_wppp$ppFE)
wppp_FE <- wppp_FE[wppp_FE<1000]
# CONFIDENCE INTERVALS ####
index_estimates <-  wppp_FE
index_estimates <- index_estimates[!is.na(index_estimates)]

# Confidence Interval for the Mean
mean_of_estimates <- mean(index_estimates, na.rm = TRUE)
sd_of_estimates <- sd(index_estimates)
n <- length(index_estimates)
error_margin <- qt(0.975, df=n-1) * sd_of_estimates/sqrt(n)
ci_mean_wppp_FE <- c(mean_of_estimates - error_margin, mean_of_estimates + error_margin)

# Bootstrap Confidence Interval for the Median
bootstrap_samples <- 10000
medians <- numeric(bootstrap_samples)

for(i in 1:bootstrap_samples) {
  sample_data <- sample(index_estimates, size=n, replace=TRUE)
  medians[i] <- median(sample_data)
}

ci_median_wppp_FE <- quantile(medians, c(0.025, 0.975))


# RESULTS ####
# wp/pp_FE
mean(wppp_FE, na.rm = TRUE)
cat("95% CI for the mean:", ci_mean_wppp_FE, "\n")
median(wppp_FE, na.rm = TRUE)
cat("95% CI for the median:", ci_median_wppp_FE, "\n")
sum(!is.na(wppp_FE))


#Between Effects ####
wppp_BE<-abs(df_wppp$wpBE/df_wppp$ppBE)
wppp_BE <- wppp_BE[wppp_BE<1000]
# CONFIDENCE INTERVALS ####
index_estimates <-  wppp_BE
index_estimates <- index_estimates[!is.na(index_estimates)]

# Confidence Interval for the Mean
mean_of_estimates <- mean(index_estimates, na.rm = TRUE)
sd_of_estimates <- sd(index_estimates)
n <- length(index_estimates)
error_margin <- qt(0.975, df=n-1) * sd_of_estimates/sqrt(n)
ci_mean_wppp_BE <- c(mean_of_estimates - error_margin, mean_of_estimates + error_margin)

# Bootstrap Confidence Interval for the Median
bootstrap_samples <- 10000
medians <- numeric(bootstrap_samples)

for(i in 1:bootstrap_samples) {
  sample_data <- sample(index_estimates, size=n, replace=TRUE)
  medians[i] <- median(sample_data)
}

ci_median_wppp_BE <- quantile(medians, c(0.025, 0.975))


# RESULTS ####
# wp/pp_BE
mean(wppp_BE, na.rm = TRUE)
cat("95% CI for the mean:", ci_mean_wppp_BE, "\n")
median(wppp_BE, na.rm = TRUE)
cat("95% CI for the median:", ci_median_wppp_BE, "\n")
sum(!is.na(wppp_BE))

all_objects <- ls()
rm(list = all_objects[!all_objects %in% c( "data", "dfreg", "avgen", "share","df_wppp",
                                           "results",   "ci_mean",         "ci_median",
                                           "resultsPP", "ci_mean_PP",      "ci_median_PP",
                                           "resultsWP", "ci_mean_WP",      "ci_median_WP",
                                           "wppp",      "ci_mean_wppp",    "ci_median_wppp",
                                           "wppp_FE",   "ci_mean_wppp_FE", "ci_median_wppp_FE",
                                           "wppp_BE",   "ci_mean_wppp_BE", "ci_median_wppp_BE")])
