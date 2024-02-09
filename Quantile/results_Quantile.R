# RESULTS 1 ####
# all, FE/BE 
mean(results[results$psi<1000,]$psi, na.rm = TRUE)
cat("95% CI for the mean:", ci_mean, "\n")
median(results$psi, na.rm = TRUE)
cat("95% CI for the median:", ci_median, "\n")
sum(!is.na(results$psi))

# published, FE/BE 
mean(resultsPP[resultsPP$psi<1000,]$psi, na.rm = TRUE)
cat("95% CI for the mean:", ci_mean_PP, "\n")
median(resultsPP$psi, na.rm = TRUE)
cat("95% CI for the median:", ci_median_PP, "\n")
sum(!is.na(resultsPP$psi))

# working papers, FE/BE 
mean(resultsWP[resultsWP$psi<1000,]$psi, na.rm = TRUE)
cat("95% CI for the mean:", ci_mean_WP, "\n")
median(resultsWP$psi, na.rm = TRUE)
cat("95% CI for the median:", ci_median_WP, "\n")
sum(!is.na(resultsWP$psi))

# RESULTS 2 ####
# WP/PP
mean(wppp[wppp<1000], na.rm = TRUE)
cat("95% CI for the mean:", ci_mean_wppp, "\n")
median(wppp, na.rm = TRUE)
cat("95% CI for the median:", ci_median_wppp, "\n")
sum(!is.na(wppp))

# wp/pp_FE
mean(wppp_FE[wppp_FE<1000], na.rm = TRUE)
cat("95% CI for the mean:", ci_mean_wppp_FE, "\n")
median(wppp_FE, na.rm = TRUE)
cat("95% CI for the median:", ci_median_wppp_FE, "\n")
sum(!is.na(wppp_FE))

# wp/pp_BE
mean(wppp_BE[wppp_BE<1000], na.rm = TRUE)
cat("95% CI for the mean:", ci_mean_wppp_BE, "\n")
median(wppp_BE, na.rm = TRUE)
cat("95% CI for the median:", ci_median_wppp_BE, "\n")
sum(!is.na(wppp_BE))