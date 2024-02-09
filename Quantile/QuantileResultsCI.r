Quantile
> # RESULTS 1 ####
> # all, FE/BE 
> mean(results[results$psi<1000,]$psi, na.rm = TRUE)
[1] 7.475225
> cat("95% CI for the mean:", ci_mean, "\n")
95% CI for the mean: -2.242459e+12 6.88642e+12 
> median(results$psi, na.rm = TRUE)
[1] 0.9928121
> cat("95% CI for the median:", ci_median, "\n")
95% CI for the median: 0.8908812 1.126963 
> sum(!is.na(results$psi))
[1] 412
> 
> # published, FE/BE 
> mean(resultsPP[resultsPP$psi<1000,]$psi, na.rm = TRUE)
[1] 9.519886
> cat("95% CI for the mean:", ci_mean_PP, "\n")
95% CI for the mean: 4.306188 14.73358 
> median(resultsPP$psi, na.rm = TRUE)
[1] 1.109492
> cat("95% CI for the median:", ci_median_PP, "\n")
95% CI for the median: 0.9569275 1.275613 
> sum(!is.na(resultsPP$psi))
[1] 407
> 
> # working papers, FE/BE 
> mean(resultsWP[resultsWP$psi<1000,]$psi, na.rm = TRUE)
[1] 16.83003
> cat("95% CI for the mean:", ci_mean_WP, "\n")
95% CI for the mean: 9.540025 24.12004 
> median(resultsWP$psi, na.rm = TRUE)
[1] 1.1924
> cat("95% CI for the median:", ci_median_WP, "\n")
95% CI for the median: 1.036948 1.583069 
> sum(!is.na(resultsWP$psi))
[1] 312
> 
> # RESULTS 2 ####
> # WP/PP
> mean(wppp[wppp<1000], na.rm = TRUE)
[1] 5.732629
> cat("95% CI for the mean:", ci_mean_wppp, "\n")
95% CI for the mean: 3.720979 7.74428 
> median(wppp, na.rm = TRUE)
[1] 1.26592
> cat("95% CI for the median:", ci_median_wppp, "\n")
95% CI for the median: 1.06367 1.613855 
> sum(!is.na(wppp))
[1] 284
> 
> # wp/pp_FE
> mean(wppp_FE[wppp_FE<1000], na.rm = TRUE)
[1] 14.78077
> cat("95% CI for the mean:", ci_mean_wppp_FE, "\n")
95% CI for the mean: 9.437791 20.12374 
> median(wppp_FE, na.rm = TRUE)
[1] 1.81221
> cat("95% CI for the median:", ci_median_wppp_FE, "\n")
95% CI for the median: 1.368146 2.119518 
> sum(!is.na(wppp_FE))
[1] 282
> 
> # wp/pp_BE
> mean(wppp_BE[wppp_BE<1000], na.rm = TRUE)
[1] 9.475066
> cat("95% CI for the mean:", ci_mean_wppp_BE, "\n")
95% CI for the mean: 5.712838 13.23729 
> median(wppp_BE, na.rm = TRUE)
[1] 1.350389
> cat("95% CI for the median:", ci_median_wppp_BE, "\n")
95% CI for the median: 1.169879 1.701806 
> sum(!is.na(wppp_BE))
[1] 288
> 