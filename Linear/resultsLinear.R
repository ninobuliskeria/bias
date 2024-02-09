# RESULTS 1 ####
# all, FE/BE 
mean(results$psi, na.rm = TRUE)
cat("95% CI for the mean:", ci_mean, "\n")
median(results$psi, na.rm = TRUE)
cat("95% CI for the median:", ci_median, "\n")
sum(!is.na(results$psi))

# published, FE/BE 
mean(resultsPP$ppPSI, na.rm = TRUE)
cat("95% CI for the mean:", ci_mean_PP, "\n")
median(resultsPP$ppPSI, na.rm = TRUE)
cat("95% CI for the median:", ci_median_PP, "\n")
sum(!is.na(resultsPP$ppPSI))

# working papers, FE/BE 
mean(resultsWP$wpPSI, na.rm = TRUE)
cat("95% CI for the mean:", ci_mean_WP, "\n")
median(resultsWP$wpPSI, na.rm = TRUE)
cat("95% CI for the median:", ci_median_WP, "\n")
sum(!is.na(resultsWP$wpPSI))

# RESULTS 2 ####
# WP/PP 
mean(wppp, na.rm = TRUE)
cat("95% CI for the mean:", ci_mean_wppp, "\n")
median(wppp, na.rm = TRUE)
cat("95% CI for the median:", ci_median_wppp, "\n")
sum(!is.na(wppp))

# wp/pp_FE 
mean(wppp_FE, na.rm = TRUE)
cat("95% CI for the mean:", ci_mean_wppp_FE, "\n")
median(wppp_FE, na.rm = TRUE)
cat("95% CI for the median:", ci_median_wppp_FE, "\n")
sum(!is.na(wppp_FE))

# wp/pp_BE 
mean(wppp_BE, na.rm = TRUE)
cat("95% CI for the mean:", ci_mean_wppp_BE, "\n")
median(wppp_BE, na.rm = TRUE)
cat("95% CI for the median:", ci_median_wppp_BE, "\n")
sum(!is.na(wppp_BE))