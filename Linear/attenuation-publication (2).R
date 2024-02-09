# attenuation-publication biases
##### Data preparation #####
rm(list=ls())

#Set directory
setwd("G:/My Drive/Education/Economics and Finance/IES/Thesis/Top-5 Project")
library("base")
library("openxlsx")
library("zoo")
library("readxl")
library("foreign")
library("xtable")
library("ggplot2")
library("dplyr")
library("shiny")
library("reshape")
library("magrittr")
library("plm")
library("lmtest")    # Recalculate model errors with sandwich functions with coeftest()
library("sandwich")  # Adjust standard errors
# library("pillar")
# 
 data <- read_excel("FinalData.xlsx", sheet = "data")
#save(data, file = "G:/My Drive/Education/Economics and Finance/IES/Thesis/Top-5 Project/final_data.RData")


#Removing meta-studies with less than 20 data points 
df <- subset(data, no_estimate_file > 19)

df$id <- seq(nrow(df))

length(df$id)

# # Creating new unique study id after removing meta-studies with less than 20 data points
# for (i in 2:length(df$id)) {
#   if (df$studyno[i]== df$studyno[i-1]) {
#     df$studyid[i]= df$studyid[i-1]
#   } else {
#     df$studyid[i] = df$studyid[i-1]+1
#   }
# }
# # Creating new unique file id after removing meta-studies with less than 20 data points
# for (i in 2:length(df$id)) {
#   if (df$ref_metaid[i]== df$ref_metaid[i-1]) {
#     df$metaid[i]= df$metaid[i-1]
#   } else {
#     df$metaid[i] = df$metaid[i-1]+1
#   }
# }

# Better alternative method
df <- df %>%
  mutate(metaid = as.integer(factor(metaid, levels = unique(metaid))), #metaIDn == metaID
         index = paste(metaid, studyno, sep = "_"),
         studyid = as.integer(factor(index, levels = unique(index))))

df <- df[,-26] # Removing th elast column: index

# unique_values <- unique(df$metaid)
# print(unique_values)
# unique_values2 <- unique(df$studyid)
# print(unique_values2)
# length(unique_values2)


# Creating dummy variables for estimates and add them to the data frame
library("fastDummies")
dummy_data <- dummy_cols(df$estimate)
df <- cbind(df,dummy_data)
df <- df[, !(colnames(df))==".data"]
df <- df[, !(colnames(df))==".data_iv"]

colnames(df)[26:30] <- c("did_iv", "experimental", "ols", "olsgroup", "other")

# In the original Excel file, experiment includes did_iv, while here I separate did_iv estimates from experimental. So, I will remove experiment and
# use only experimental!
which(colnames(df)=="did_iv" )
table(df$experiment==df$experimental)["FALSE"]
# sum(df[26])
df <- df[, !(colnames(df))=="experiment"]
# Moving dummy variables to the middle of the data frame
which(colnames(df)=="iv")
df <- cbind(df[c(1:12)], df[c(25:29)],df[c(13:24)])

# saving to STATA
# dff <- df[,-c(4,9,10,11)]
# write.dta(dff, "final_df.dta")



##### Simple mean comparison #####

# Different means
mean_file <- df %>% 
  group_by(metaid) %>% 
  summarize(mean= mean(effect))

means_methods <- df %>% 
  group_by(estimate) %>% 
  summarize(mean= mean(effect))

mean_file_method <- df %>% 
  group_by(metaid,estimate) %>% 
  summarize(mean= mean(effect))

sd_file_method <- df %>% 
  group_by(metaid,estimate) %>% 
  summarize(sd= sd(effect))

mean_iv <- data.frame(subset(mean_file_method, estimate == "iv"))
mean_ols <- data.frame(subset(mean_file_method, estimate == "ols"))
mean_olsgroup <- data.frame(subset(mean_file_method, estimate == "olsgroup"))
mean_exp <- data.frame(subset(mean_file_method, estimate == "experimental"))

# iv vs ols
both_ivols <- merge(mean_iv,mean_ols,by="metaid", all=FALSE)


compare_ivols <- data.frame(both_ivols[,3] > both_ivols[,5])
compare_ivols <- cbind(both_ivols$metaid,compare_ivols)
colnames(compare_ivols) <- c("metaid","IV > OLS")
summary(compare_ivols[,2])

# iv vs olsgroup
both_ivolsgroup <- merge(mean_iv,mean_olsgroup,by="metaid", all=FALSE)

compare_ivolsgroup <- data.frame(both_ivolsgroup[,3] > both_ivolsgroup[,5])
compare_ivolsgroup <- cbind(both_ivolsgroup$metaid,compare_ivolsgroup)
colnames(compare_ivolsgroup) <- c("metaid","IV > OLS Group")
summary(compare_ivolsgroup[,2])

# ols vs experimental
both_olsexp <- merge(mean_ols,mean_exp,by="metaid", all=FALSE)

compare_olsexp <- data.frame(both_olsexp[,3] > both_olsexp[,5])
compare_olsexp <- cbind(both_olsexp$metaid,compare_olsexp)
colnames(compare_olsexp) <- c("metaid","OLS > Experimental")
summary(compare_olsexp[,2])


##### Simple median comparison #####

# Different medians
median_file <- df %>% 
  group_by(metaid) %>% 
  summarize(median= median(effect))

medians_methods <- df %>% 
  group_by(estimate) %>% 
  summarize(median= median(effect))

median_file_method <- df %>% 
  group_by(metaid,estimate) %>% 
  summarize(median= median(effect))

sd_file_method <- df %>% 
  group_by(metaid,estimate) %>% 
  summarize(sd= sd(effect))

median_iv <- data.frame(subset(median_file_method, estimate == "iv"))
median_ols <- data.frame(subset(median_file_method, estimate == "ols"))
median_olsgroup <- data.frame(subset(median_file_method, estimate == "olsgroup"))
median_exp <- data.frame(subset(median_file_method, estimate == "experimental"))

# iv vs ols
both_median_ivols <- merge(median_iv,median_ols,by="metaid", all=FALSE)


compare_median_ivols <- data.frame(both_median_ivols[,3] > both_median_ivols[,5])
compare_median_ivols <- cbind(both_median_ivols$metaid,compare_median_ivols)
colnames(compare_median_ivols) <- c("metaid","IV > OLS")
summary(compare_median_ivols[,2])


# iv vs olsgroup
both_median_ivolsgroup <- merge(median_iv,median_olsgroup,by="metaid", all=FALSE)

compare_median_ivolsgroup <- data.frame(both_median_ivolsgroup[,3] > both_median_ivolsgroup[,5])
compare_median_ivolsgroup <- cbind(both_median_ivolsgroup$metaid,compare_median_ivolsgroup)
colnames(compare_median_ivolsgroup) <- c("metaid","IV > OLS Group")
summary(compare_median_ivolsgroup[,2])


# ols vs experimental
both_median_olsexp <- merge(median_ols,median_exp,by="metaid", all=FALSE)

compare_median_olsexp <- data.frame(both_median_olsexp[,3] > both_median_olsexp[,5])
compare_median_olsexp <- cbind(both_median_olsexp$metaid,compare_median_olsexp)
colnames(compare_median_olsexp) <- c("metaid","OLS > Experimental")
summary(compare_median_olsexp[,2])


##### PEESE #####

# creating precison variable
df$se[df$se==0] <- 0.0001
df$prec <- 1/df$se
df$ts <- df$effect/df$se

# OLS regressions
df_ols <- data.frame(subset(df, ols==1))
olssplit <- split(df_ols, df_ols$metaid) #split the data set based on the number of meta-studies
olssplit <- lapply(olssplit, function(df_ols) lm(ts ~ se + prec - 1, data = df_ols))

# OLS group regressions
df_olsgroup <- data.frame(subset(df, olsgroup==1))
olsgroupsplit <- split(df_olsgroup, df_olsgroup$metaid) #split the data set based on the number of meta-studies
olsgroupsplit <- lapply(olsgroupsplit, function(df_olsgroup) lm(ts ~ se + prec - 1, data = df_olsgroup))

# All OLS regressions
df_allols <- data.frame(subset(df, olsgroup==1| ols==1))
allolssplit <- split(df_allols, df_allols$metaid) #split the data set based on the number of meta-studies
allolssplit <- lapply(allolssplit, function(df_allols) lm(ts ~ se + prec - 1, data = df_allols))

# IV regressions
df_iv <- data.frame(subset(df, iv==1))
ivsplit <- split(df_iv, df_iv$metaid) #split the data set based on the number of meta-studies
ivsplit <- lapply(ivsplit, function(df_iv) lm(ts ~ se + prec - 1, data = df_iv))

# Experimental regressions
df_exp <- data.frame(subset(df, experimental==1))
expsplit <- split(df_exp, df_exp$metaid) #split the data set based on the number of meta-studies
expsplit <- lapply(expsplit, function(df_exp) lm(ts ~ se + prec - 1, data = df_exp))

# List of first step of PEESE results
peese_list <- list("ols"=olssplit,"olsgroup"=olsgroupsplit, 
                   "iv"=ivsplit, "experimental"=expsplit
                   ,"allols"=allolssplit
                   )
# Making an empty list for final PEESE results
peese_results_list <- peese_list

peese_error_all <- vector("list", length(peese_list))

# Loop for all PEESE final results
for (i in 1:length(peese_list)) {
  for (g in 1:length(peese_list[[i]])) {
    result <- try({
      peese_results_list[[i]][[g]] <- as.data.frame(coeftest(peese_list[[i]][[g]], vcov = vcovCL, type = "HC1", cluster = ~studyno)[,])
    }, silent = TRUE)
    if (inherits(result, "try-error")) {
      peese_results_list[[i]][[g]] <- NA  # Assign NA if there is an error
      peese_error_all[[i]] <- c(peese_error_all[[i]],g)  # Store the index of the error
      warning(paste("Error occurred in iteration", g, ": ", attr(result, "message")))
    }
  }
}

# Naming the PEESE error list
names(peese_error_all) <- c("ols","olsgroup","iv","experimental"
                            ,"allols"
                            ) 


##### Andrews & Kasy 2019 #####

 # options(repos = c(skranz = 'https://skranz.r-universe.dev',
 #                   CRAN = 'https://cloud.r-project.org'))
 # install.packages('MetaStudies')
library(MetaStudies)

# creating precison variable
 df$se[df$se==0] <- 0.0001
# df$prec <- 1/df$se
# df$ts <- df$effect/df$se

# OLS 
df_ols <- data.frame(subset(df, ols==1))
olslist <- split(df_ols, df_ols$metaid) #split the data set based on the number of meta-studies

# OLS group
df_olsgroup <- data.frame(subset(df, olsgroup==1))
olsgrouplist <- split(df_olsgroup, df_olsgroup$metaid) #split the data set based on the number of meta-studies

# All OLS 
# df_allols <- data.frame(subset(df, ols==1 | olsgroup==1))
# allolslist <- split(df_allols, df_allols$metaid) #split the data set based on the number of meta-studies

# IV 
df_iv <- data.frame(subset(df, iv==1))
ivlist <- split(df_iv, df_iv$metaid) #split the data set based on the number of meta-studies

# Experimental 
df_exp <- data.frame(subset(df, experimental==1))
explist <- split(df_exp, df_exp$metaid) #split the data set based on the number of meta-studies

# List of sub data sets for AK 2019 test
ak_list <- list("ols"=olslist,"olsgroup"=olsgrouplist, 
                "iv"=ivlist, "experimental"=explist
                 # ,"allols"=allolslist
                )

# Indices of effect and se 
ind <- c("se", "effect")
which(names(ak_list[[1]][[2]]) %in%  ind) # 2 is se and 3 is effect, hence in AK test X is index 3 and sigma is index 2

ak_results <- ak_list
ak_error_all <- vector("list", length(ak_list)) 
names(ak_error_all) <- c("ols","olsgroup","iv","experimental"
                         # ,"allols"
                         ) 

for (g in 1:length(ak_list)) {
  for (i in 1:length(ak_list[[g]])) {
  result <- try(metastudies_estimation(X = ak_list[[g]][[i]]$effect, sigma = ak_list[[g]][[i]]$se, model = "t", cutoffs = c(1.96), symmetric = TRUE), silent = TRUE)
  if (inherits(result, "try-error")) {
    # Store the index of the error
    ak_error_all[[g]] <- c(ak_error_all[[g]], i)
    # Print or handle the error message as desired
    # print(paste("Error occurred in iteration", i))
  } else {
    ak_results[[g]][[i]] <- result
  }
}
}

save(ak_results, file = "ak_list_data.RData")

##### Collecting indices ##### 

#* From simple mean comparison ----

mean_index_results <- c()

mean_index_results[[1]] <- both_ivols[,1]
mean_index_results[[2]] <- both_ivolsgroup[,1]
mean_index_results[[3]] <- both_olsexp[,1]
names(mean_index_results) <- c("ivols","ivolsgroup","olsexp")

#* From simple median comparison ----

median_index_results <- c()

median_index_results[[1]] <- both_median_ivols[,1]
median_index_results[[2]] <- both_median_ivolsgroup[,1]
median_index_results[[3]] <- both_median_olsexp[,1]
names(median_index_results) <- c("ivols","ivolsgroup","olsexp")


#* From PEESE (Errors) ----

# Creating new lists of results without problematic components
new_peese_results_list <- peese_results_list

new_peese_results_list$ols <-  peese_results_list$ols[-peese_error_all$ols]
new_peese_results_list$ols <- new_peese_results_list$ols[sapply(new_peese_results_list$ols, function(x) !anyNA(unlist(x)))]

new_peese_results_list$olsgroup <- peese_results_list$olsgroup[-peese_error_all$olsgroup]
new_peese_results_list$olsgroup <- new_peese_results_list$olsgroup[sapply(new_peese_results_list$olsgroup, function(x) !anyNA(unlist(x)))]
new_peese_results_list$olsgroup <- new_peese_results_list$olsgroup[sapply(new_peese_results_list$olsgroup, function(x) !any(is.infinite(unlist(x))))]

new_peese_results_list$iv <- peese_results_list$iv[-peese_error_all$iv]
new_peese_results_list$iv  <- lapply(new_peese_results_list$iv , function(dd) {
  if (ncol(dd) == 4) {
    return(dd)
  }
})
new_peese_results_list$iv  <- Filter(Negate(is.null), new_peese_results_list$iv )
new_peese_results_list$iv <- new_peese_results_list$iv[sapply(new_peese_results_list$iv, function(x) !anyNA(unlist(x)))]
new_peese_results_list$iv <- new_peese_results_list$iv[sapply(new_peese_results_list$iv, function(x) !any(is.infinite(unlist(x))))]

new_peese_results_list$experimental <- peese_results_list$experimental[-peese_error_all$experimental]
new_peese_results_list$experimental <- new_peese_results_list$experimental[sapply(new_peese_results_list$experimental, function(x) !anyNA(unlist(x)))]
new_peese_results_list$experimental <- new_peese_results_list$experimental[sapply(new_peese_results_list$experimental, function(x) !any(is.infinite(unlist(x))))]

new_peese_results_list$allols <- peese_results_list$allols[-peese_error_all$allols]
new_peese_results_list$allols <- new_peese_results_list$allols[sapply(new_peese_results_list$allols, function(x) !anyNA(unlist(x)))]
new_peese_results_list$allols <- new_peese_results_list$allols[sapply(new_peese_results_list$allols, function(x) !any(is.infinite(unlist(x))))]



#Extracting results for each group and transfer them to a data frame
peese_final <- vector("list", length(new_peese_results_list))
names(peese_final) <- c("ols","olsgroup","iv","experimental"
                        ,"allols"
                        )
for (i in 1:length(peese_final)) {
  peese_final[[i]] <- data.frame(matrix(ncol = 7, nrow = 2*length(new_peese_results_list[[i]])))
  colnames(peese_final[[i]])[1] <- c("Meta id")
  peese_final[[i]][,1] <- rep(as.integer(names(new_peese_results_list[[i]])),each=2)
  colnames(peese_final[[i]])[2] <- "Method"
  peese_final[[i]][,2] <- names(peese_final)[i]
  colnames(peese_final[[i]])[3] <- "Coefficient"
  peese_final[[i]][,3] <- rep(rev(c(rownames(new_peese_results_list$ols[[1]]))),times=length(new_peese_results_list[[i]]))
  colnames(peese_final[[i]])[4:7] <- colnames(as.matrix(new_peese_results_list[[i]][[1]]))
}


for (g in 1:length(peese_final)) {
  for (i in 1:length(new_peese_results_list[[g]])) {
  peese_final[[g]][((2*i)-1), 4:7] <- new_peese_results_list[[g]][[i]][2, 1:4]
  # for (j in 1:length(new_peese_results_list[[g]])) {
  peese_final[[g]][(2*i), 4:7] <- new_peese_results_list[[g]][[i]][1, 1:4]
  # }
}
}


# Extracting shared meta-studies for iv vs ols
common_metaid <- peese_final$ols[,1][peese_final$ols[,1] %in% peese_final$iv[,1]]
common_values <- peese_final$ols[,1][peese_final$ols[,1] %in% peese_final$iv[,1] & duplicated(peese_final$ols[,1])]
peese_final_ols <- data.frame(peese_final$ols)
peese_final_ols <- peese_final_ols[peese_final_ols[, 1] %in% common_metaid,]
peese_final_iv <- data.frame(peese_final$iv)
peese_final_iv <- peese_final_iv[peese_final_iv[,1] %in% common_metaid,]
peese_ivols <- cbind(peese_final_iv, peese_final_ols)

# Creating the final table only for corrected means and their standard errors
# Version 1
peese_ivols_new <- peese_ivols[peese_ivols$Coefficient=="prec", -c(6,7,8,10,13,14)]
colnames(peese_ivols_new)[c(4,7)] <- c("Corrected_Mean_IV","Corrected_Mean_OLS")
colnames(peese_ivols_new)[c(5,8)] <- c("S.E.IV","S.E.OLS") 
peese_ivols_new <- peese_ivols_new[, -c(2,3,6)]

# Version 2
peese_ivols_new <- peese_ivols[, -c(6,7,8,10,13,14)]
peese_ivols_new[seq(2, nrow(peese_ivols_new ), by = 2), c(4:8) ] <- NA
peese_ivols_new[,4] <- ifelse(is.na(peese_ivols_new[,4]), peese_ivols_new[,5], peese_ivols_new[,4])
for (g in c(4,7)) {
for (i in 1:length(peese_ivols_new[,1])) {
  if (is.na(peese_ivols_new[,g][i])) {
    peese_ivols_new[,g][i] <- peese_ivols_new[,g+1][i-1]
  } else {
    peese_ivols_new[,g][i] <- peese_ivols_new[,g][i]
  }
}
}
colnames(peese_ivols_new)[c(1,4,7)] <- c("Meta id","IV","OLS")
peese_ivols_new[,3] <- rep(c("Corrected Mean","S.E."), times=length(peese_ivols_new[,3])/2)
peese_ivols_new <- peese_ivols_new[, -c(2,5,6,8)]



# Extracting shared meta-studies for ols vs experimental
common_metaid <- peese_final$experimental[,1][peese_final$experimental[,1] %in% peese_final$ols[,1]]
common_values <- peese_final$experimental[,1][peese_final$experimental[,1] %in% peese_final$ols[,1] & duplicated(peese_final$experimental[,1])]
peese_final_ols <- data.frame(peese_final$ols)
peese_final_ols <- peese_final_ols[peese_final_ols[, 1] %in% common_metaid,]
peese_final_experimental <- data.frame(peese_final$experimental)
peese_final_experimental <- peese_final_experimental[peese_final_experimental[,1] %in% common_metaid,]
peese_olsexp <- cbind(peese_final_ols, peese_final_experimental)

# Creating the final table only for corrected means and their standard errors
# Version 1
peese_olsexp_new <- peese_olsexp[peese_olsexp$Coefficient=="prec", -c(6,7,8,10,13,14)]
colnames(peese_olsexp_new)[c(4,7)] <- c("Corrected_Mean_OLS","Corrected_Mean_Experimental")
colnames(peese_olsexp_new)[c(5,8)] <- c("S.E.OLS","S.E.Experimental") 
peese_olsexp_new <- peese_olsexp_new[, -c(2,3,6)]

# Version 2
peese_olsexp_new <- peese_olsexp[, -c(6,7,8,10,13,14)]
peese_olsexp_new[seq(2, nrow(peese_olsexp_new ), by = 2), c(4:8) ] <- NA
peese_olsexp_new[,4] <- ifelse(is.na(peese_olsexp_new[,4]), peese_olsexp_new[,5], peese_olsexp_new[,4])
for (g in c(4,7)) {
  for (i in 1:length(peese_olsexp_new[,1])) {
    if (is.na(peese_olsexp_new[,g][i])) {
      peese_olsexp_new[,g][i] <- peese_olsexp_new[,g+1][i-1]
    } else {
      peese_olsexp_new[,g][i] <- peese_olsexp_new[,g][i]
    }
  }
}
colnames(peese_olsexp_new)[c(1,4,7)] <- c("Meta id","OLS","Experimental")
peese_olsexp_new[,3] <- rep(c("Corrected Mean","S.E."), times=length(peese_olsexp_new[,3])/2)
peese_olsexp_new <- peese_olsexp_new[, -c(2,5,6,8)]





#* From AK (Errors) ---------------------

# Creating new lists of results without problematic components
new_ak_results <- ak_results
new_ak_results$ols <-  ak_results$ols[-ak_error_all$ols]
new_ak_results$ols <- new_ak_results$ols[sapply(new_ak_results$ols, function(x) !anyNA(unlist(x)))]
new_ak_results$ols <- new_ak_results$ols[sapply(new_ak_results$ols, function(x) !any(is.infinite(unlist(x))))]
new_ak_results$olsgroup <- ak_results$olsgroup[-ak_error_all$olsgroup]
new_ak_results$iv <- ak_results$iv[-ak_error_all$iv]
new_ak_results$experimental <- ak_results$experimental[-ak_error_all$experimental]
# new_ak_results$allols <- ak_results$allols[-ak_error_all$allols]

# Extracting results for each group and transfer them to a data frame
ak_final <- vector("list", length(new_ak_results))
names(ak_final) <- c("ols","olsgroup","iv","experimental"
                        # ,"allols"
                     )

for (i in 1:length(ak_final)) {
  ak_final[[i]] <- data.frame(matrix(ncol = 7, nrow = 2*length(new_ak_results[[i]])))
  colnames(ak_final[[i]])[1] <- c("Meta id")
  ak_final[[i]][,1] <- rep(as.integer(names(new_ak_results[[i]])),each=2)
  colnames(ak_final[[i]])[2] <- "Method"
  ak_final[[i]][,2] <- names(ak_final[i])
  colnames(ak_final[[i]])[3] <- "Coefficient"
  ak_final[[i]][,3] <- rep(c(rownames(new_ak_results$ols[[1]]$est_tab)),times=length(new_ak_results[[i]]))
  colnames(ak_final[[i]])[4:7] <- colnames(new_ak_results$ols[[1]]$est_tab)
}

for (g in 1:length(ak_final)) {
  for (i in 1:length(new_ak_results[[g]])) {
    ak_final[[g]][((2*i)-1), 4:7] <- new_ak_results[[g]][[i]]$est_tab[1, 1:4]
    # for (j in 1:length(new_peese_results_list[[g]])) {
    ak_final[[g]][(2*i), 4:7] <- new_ak_results[[g]][[i]]$est_tab[2, 1:4]
    # }
  }
}


#*---- Extracting shared meta-studies for iv vs ols ----
common_metaid <- ak_final$ols[,1][ak_final$ols[,1] %in% ak_final$iv[,1]]
common_values <- ak_final$ols[,1][ak_final$ols[,1] %in% ak_final$iv[,1] & duplicated(ak_final$ols[,1])]
ak_final_ols <- data.frame(ak_final$ols)
ak_final_ols <- ak_final_ols[ak_final_ols[, 1] %in% common_metaid,]
ak_final_iv <- data.frame(ak_final$iv)
ak_final_iv <- ak_final_iv[ak_final_iv[,1] %in% common_metaid,]
ak_ivols <-cbind(ak_final_iv, ak_final_ols)
# Creating the final table only for corrected means and their standard errors
ak_ivols_new <- ak_ivols[, -c(2,5,6,7,8,9,10,12,13,14)]
colnames(ak_ivols_new)[c(1,3,4)] <- c("Meta id","IV","OLS")
ak_ivols_new[,2] <-  rep(c("Corrected Mean","S.E."))

#*---- Extracting shared meta-studies for ols vs experimental ----
common_metaid <- ak_final$experimental[,1][ak_final$experimental[,1] %in% ak_final$ols[,1]]
common_values <- ak_final$experimental[,1][ak_final$experimental[,1] %in% ak_final$ols[,1] & duplicated(ak_final$experimental[,1])]
ak_final_ols <- data.frame(ak_final$ols)
ak_final_ols <- ak_final_ols[ak_final_ols[, 1] %in% common_metaid,]
ak_final_experimental <- data.frame(ak_final$experimental)
ak_final_experimental <- ak_final_experimental[ak_final_experimental[,1] %in% common_metaid,]
ak_olsexp <-cbind(ak_final_ols, ak_final_experimental)
# Creating the final table only for corrected means and their standard errors
ak_olsexp_new <- ak_olsexp[, -c(2,5,6,7,8,9,10,12,13,14)]
colnames(ak_olsexp_new)[c(1,3,4)] <- c("Meta id","OLS","Experimental")
ak_olsexp_new[,2] <-  rep(c("Corrected Mean","S.E."))


#*---- Calling simple mean results ----
# iv vs ols
simple_ivols<- cbind(both_ivols,compare_ivols)
simple_ivols <- simple_ivols[,-c(2,4,6)]
# adding number of observations
obs_ivols <- as.vector(simple_ivols[,1])
obsiv <- sapply(obs_ivols, function(i) {
  length(df_iv$metaid[df_iv$metaid == i])
})
obsols_foriv <- sapply(obs_ivols, function(i) {
  length(df_ols$metaid[df_ols$metaid == i])
})
simple_ivols <- cbind(simple_ivols[,1:2], obsiv,simple_ivols[,3], obsols_foriv, simple_ivols[,4])
colnames(simple_ivols) <- c("Meta id","IV","Obs (IV)","OLS","Obs (OLS)" ,"IV > OLS")  


# ols vs experimental
simple_olsexp<- cbind(both_olsexp,compare_olsexp)
simple_olsexp <- simple_olsexp[,-c(2,4,6)]
# adding number of observations
obs_olsexp <- as.vector(simple_olsexp[,1])
obsols_forexp <- sapply(obs_olsexp, function(i) {
  length(df_ols$metaid[df_ols$metaid == i])
})
obsexp <- sapply(obs_olsexp, function(i) {
  length(df_exp$metaid[df_exp$metaid == i])
})
simple_olsexp<- cbind(simple_olsexp[,1:2], obsols_forexp,simple_olsexp[,3], obsexp, simple_olsexp[,4])
colnames(simple_olsexp) <- c("Meta id","OLS","Obs (OLS)","Experimental","Obs (Experimental)","OLS > Experimental")



#*---- Calling simple median results ----
# iv vs ols
simple_median_ivols<- cbind(both_median_ivols,compare_median_ivols)
simple_median_ivols <- simple_median_ivols[,-c(2,4,6)]
# adding number of observations
obs_ivols <- as.vector(simple_median_ivols[,1])
obsiv <- sapply(obs_ivols, function(i) {
  length(df_iv$metaid[df_iv$metaid == i])
})
obsols_foriv <- sapply(obs_ivols, function(i) {
  length(df_ols$metaid[df_ols$metaid == i])
})
simple_median_ivols <- cbind(simple_median_ivols[,1:2], obsiv,simple_median_ivols[,3], obsols_foriv, simple_median_ivols[,4])
colnames(simple_median_ivols) <- c("Meta id","IV","Obs (IV)","OLS","Obs (OLS)" ,"IV > OLS")  


# ols vs experimental
simple_median_olsexp<- cbind(both_median_olsexp,compare_median_olsexp)
simple_median_olsexp <- simple_median_olsexp[,-c(2,4,6)]
# adding number of observations
obs_olsexp <- as.vector(simple_median_olsexp[,1])
obsols_forexp <- sapply(obs_olsexp, function(i) {
  length(df_ols$metaid[df_ols$metaid == i])
})
obsexp <- sapply(obs_olsexp, function(i) {
  length(df_exp$metaid[df_exp$metaid == i])
})
simple_median_olsexp<- cbind(simple_median_olsexp[,1:2], obsols_forexp,simple_median_olsexp[,3], obsexp, simple_median_olsexp[,4])
colnames(simple_median_olsexp) <- c("Meta id","OLS","Obs (OLS)","Experimental","Obs (Experimental)","OLS > Experimental")

#**---- Final table for iv vs ols ----
common_ak_peese_ivols <- ak_ivols_new[,1][ak_ivols_new[,1] %in% peese_ivols_new[,1]]
unique_ak_peese_ivols <- unique(common_ak_peese_ivols)
ak_ivols_new2 <- ak_ivols_new[ak_ivols_new[, 1] %in% common_ak_peese_ivols,]
peese_ivols_new2 <- peese_ivols_new[peese_ivols_new[,1] %in% common_ak_peese_ivols,]
peese_ak_ivols <-cbind(peese_ivols_new2 , ak_ivols_new2)
peese_ak_ivols <- peese_ak_ivols[,-c(5,6)]  #contains results for both PEESE and AK for IV vs OLS
# getting number of observations
unique_peese_ak<- unique(peese_ak_ivols[,1])
obsiv_final <- sapply(unique_peese_ak, function(i) {
  length(df_iv$metaid[df_iv$metaid == i])
})
obsols_foriv_final <- sapply(unique_peese_ak, function(i) {
  length(df_ols$metaid[df_ols$metaid == i])
})

# Adding empty rows
# Determine the row indices for the empty rows starting from the third row
empty_row_indices <- seq(3, nrow(peese_ak_ivols), by = 2)
# Create an empty data frame with the same column structure as the original data frame
empty_row <- data.frame(matrix(NA, nrow = 1, ncol = ncol(peese_ak_ivols)))
colnames(empty_row) <- colnames(peese_ak_ivols)
# Create a new data frame with empty rows inserted
new_df <- peese_ak_ivols
insert_indices <- sort(c(empty_row_indices), decreasing = TRUE)
for (i in insert_indices) {
  new_df <- rbind(new_df[1:(i-1), ], empty_row, new_df[i:nrow(new_df), ])
}
peese_ak_ivols <- rbind(new_df,empty_row)
# Reset row names
rownames(peese_ak_ivols) <- NULL

# Adding number of observations
for (i in 1:length(obsiv_final)) {
  peese_ak_ivols[,3][(4*i)-i] <- format(obsiv_final[i], nsmall = 2)
  peese_ak_ivols[,4][(4*i)-i] <- format(obsols_foriv_final[i], nsmall = 2)
  peese_ak_ivols[,5][(4*i)-i] <- format(obsiv_final[i], nsmall = 2)
  peese_ak_ivols[,6][(4*i)-i] <- format(obsols_foriv_final[i], nsmall = 2)
}
# Remove values every 3 cells starting from row 2 in the specified columns
remove_indices <- seq(2, nrow(peese_ak_ivols), by = 3)
peese_ak_ivols[seq(2, nrow(peese_ak_ivols), by = 3), "Meta id"] <- NA

colnames(peese_ak_ivols) <- c("Meta id","Coefficient","IV","OLS",
                               "IV2","OLS2")
for (i in 3:6) {
  peese_ak_ivols[,i] <- as.numeric(peese_ak_ivols[,i])  
}

# for printing
peese_ak_ivols[, 6][137] <-0.0065394
peese_ak_ivols2 <-peese_ak_ivols
for (i in 3:6) {
  peese_ak_ivols2[,i] <- as.numeric(peese_ak_ivols2[,i])  
}
notround <- seq(from = 3, to = 234, by = 3)
peese_ak_ivols2 <- peese_ak_ivols2 %>%
  mutate_if(is.numeric, ~ ifelse(row_number() %in% notround, format(round(., digits = 0), nsmall = 0), .))
for (i in 3:6) {
  peese_ak_ivols2[,i] <- as.numeric(peese_ak_ivols2[,i])  
}
peese_ak_ivols2 <- peese_ak_ivols2 %>%
  mutate(across(3:6, ~ format(round(., digits = 3), nsmall = 3)))
for (i in 3:6) {
  peese_ak_ivols2[,i] <- as.numeric(peese_ak_ivols2[,i])  
}

cellsc<- seq(from = 3, to = 234, by = 3)
peese_ak_ivols2[,2][cellsc] = "Obs"

# Excel file
write.xlsx(peese_ak_ivols2, file = "peese_ak_ivols.xlsx", sheetName = "Sheet1", rowNames = FALSE)

bracket <- seq(from = 2, to = 230, by = 3)
for (i in 3:6) {
  peese_ak_ivols2[,i][bracket] <- paste0("(",peese_ak_ivols2[,i][bracket],")",sep = "")
}

table_latex_peas_ak_ivols <- xtable(peese_ak_ivols2, include.rownames = FALSE, digits = 3)
print.xtable(table_latex_peas_ak_ivols,include.rownames = FALSE)





#**---- Final table for ols vs experimental ----
common_ak_peese_olsexp <- peese_olsexp_new[,1][peese_olsexp_new[,1] %in% ak_olsexp_new[,1]]
unique_ak_peese_olsexp <- unique(common_ak_peese_olsexp)
ak_olsexp_new2 <- ak_olsexp_new[ak_olsexp_new[, 1] %in% common_ak_peese_olsexp,]
peese_olsexp_new2 <- peese_olsexp_new[peese_olsexp_new[,1] %in% common_ak_peese_olsexp,]
peese_ak_olsexp <-cbind(peese_olsexp_new2 , ak_olsexp_new2) 
peese_ak_olsexp <- peese_ak_olsexp[,-c(5,6)] #contains results for both PEESE and AK for OLS vs Experimental
# getting number of observations
unique_peese_ak<- unique(peese_ak_olsexp[,1])
obsols_forexp_final <- sapply(unique_peese_ak, function(i) {
  length(df_ols$metaid[df_ols$metaid == i])
})
obsexp_final <- sapply(unique_peese_ak, function(i) {
  length(df_exp$metaid[df_exp$metaid == i])
})

# Adding empty rows
# Determine the row indices for the empty rows starting from the third row
empty_row_indices <- seq(3, nrow(peese_ak_olsexp), by = 2)
# Create an empty data frame with the same column structure as the original data frame
empty_row <- data.frame(matrix(NA, nrow = 1, ncol = ncol(peese_ak_olsexp)))
colnames(empty_row) <- colnames(peese_ak_olsexp)
# Create a new data frame with empty rows inserted
new_df <- peese_ak_olsexp
insert_indices <- sort(c(empty_row_indices), decreasing = TRUE)
for (i in insert_indices) {
  new_df <- rbind(new_df[1:(i-1), ], empty_row, new_df[i:nrow(new_df), ])
}
peese_ak_olsexp <- rbind(new_df,empty_row)
# Reset row names
rownames(peese_ak_olsexp) <- NULL

for (i in 1:length(obsols_forexp_final)) {
    peese_ak_olsexp[,3][(4*i)-i] <- as.numeric(format(obsols_forexp_final[i], nsmall = 2))
    peese_ak_olsexp[,4][(4*i)-i] <- as.numeric(format(obsexp_final[i], nsmall = 2))
    peese_ak_olsexp[,5][(4*i)-i] <- as.numeric(format(obsols_forexp_final[i], nsmall = 2))
    peese_ak_olsexp[,6][(4*i)-i] <- as.numeric(format(obsexp_final[i], nsmall = 2))
}

# Remove values every 3 cells starting from row 2 in the specified columns
remove_indices <- seq(2, nrow(peese_ak_olsexp), by = 3)
peese_ak_olsexp[seq(2, nrow(peese_ak_olsexp), by = 3), "Meta id"] <- NA

colnames(peese_ak_olsexp) <- c("Meta id","Coefficient","OLS","Experimental",
                                "OLS-","Experimental-")
for (i in 3:6) {
  peese_ak_olsexp[,i] <- as.numeric(peese_ak_olsexp[,i])  
}


# for printing
peese_ak_olsexp2 <- peese_ak_olsexp
for (i in 3:6) {
  peese_ak_olsexp2[,i] <- as.numeric(peese_ak_olsexp2[,i])  
}
notround <- seq(from = 3, to = 24, by = 3)
peese_ak_olsexp2 <- peese_ak_olsexp2 %>%
  mutate_if(is.numeric, ~ ifelse(row_number() %in% notround, format(round(., digits = 0), nsmall = 0), .))
for (i in 3:6) {
  peese_ak_olsexp2[,i] <- as.numeric(peese_ak_olsexp2[,i])  
}
peese_ak_olsexp2 <- peese_ak_olsexp2 %>%
  mutate(across(3:6, ~ format(round(., digits = 3), nsmall = 3)))
for (i in 3:6) {
  peese_ak_olsexp2[,i] <- as.numeric(peese_ak_olsexp2[,i])  
}

cellsc<- seq(from = 3, to = 24, by = 3)
peese_ak_olsexp2[,2][cellsc] = "Obs"

# Excel file
write.xlsx(peese_ak_olsexp2, file = "peese_ak_olsexp.xlsx", sheetName = "Sheet1", rowNames = FALSE)

bracket <- seq(from = 2, to = 23, by = 3)
for (i in 3:6) {
  peese_ak_olsexp2[,i][bracket] <- paste0("(",peese_ak_olsexp2[,i][bracket],")",sep = "")
}

table_latex_peese_ak_olsexp <- xtable(peese_ak_olsexp2, include.rownames = FALSE, digits = 3)
print.xtable(table_latex_peese_ak_olsexp,include.rownames = FALSE)

#**---- Ratios ----


# empty vectors
ratio_peese_ivols<-vector()
ratio_ak_ivols<-vector()
ratio_peese_olsexp<-vector()
ratio_ak_olsexp<-vector()

#PEESE iv vs ols

for (i in 1:length(peese_ak_ivols[,4])) {
  ratio_peese_ivols[i] <- ((peese_ak_ivols[,3][i]) - (peese_ak_ivols[,4][i]))/(abs(peese_ak_ivols[,4][i]))
}
ratio_peese_ivols <- cbind(peese_ak_ivols[,1],ratio_peese_ivols)
# ratio_peese_ivols <- ratio_peese_ivols[seq(1, nrow(ratio_peese_ivols), by = 2),]
ratio_peese_ivols <- ratio_peese_ivols[complete.cases(ratio_peese_ivols), ]

#adding the number of observations to each ratio
ratio_peese_ivols <- cbind(ratio_peese_ivols,obsiv_final,obsols_foriv_final)
ratio_peese_ivols[,2] <- as.numeric(round(ratio_peese_ivols[,2], 3))
colnames(ratio_peese_ivols) <- c("Meta Id", "Ratio", "Obs (IV)", "Obs (OLS)")

ratio_peese_ivols2 <-ratio_peese_ivols[-c(4,44),] #removing outliers
mean_ratio_peese_ivols <- mean(ratio_peese_ivols2[,2])
sd_ratio_peese_ivols <- sd(ratio_peese_ivols2[,2])
mean_ratio_peese_ivols
sd_ratio_peese_ivols


median_ratio_peese_ivols <- median(ratio_peese_ivols2[,2])

median_ratio_peese_ivols

# AK iv vs ols
for (i in 1:length(peese_ak_ivols[,4])) {
  ratio_ak_ivols[i] <- ((peese_ak_ivols[,5][i])-(peese_ak_ivols[,6][i]))/(abs(peese_ak_ivols[,6][i]))
}
ratio_ak_ivols <- cbind(peese_ak_ivols[,1],ratio_ak_ivols)
ratio_ak_ivols <- ratio_ak_ivols[complete.cases(ratio_ak_ivols), ]

#adding the number of observations to each ratio
ratio_ak_ivols <- cbind(ratio_ak_ivols,obsiv_final,obsols_foriv_final)
ratio_ak_ivols[,2] <- as.numeric(round(ratio_ak_ivols[,2], 3))
colnames(ratio_ak_ivols) <- c("Meta id", "Ratio", "Obs (IV)", "Obs (OLS)")

ratio_ak_ivols2 <-ratio_ak_ivols[-c(4,44),] #removing outliers
mean_ratio_ak_ivols <- mean(ratio_ak_ivols2[,2])
sd_ratio_ak_ivols <- sd(ratio_ak_ivols2[,2])
mean_ratio_ak_ivols 
sd_ratio_ak_ivols

sum(ratio_ak_ivols2[,4])

ratio_peese_ak_ivols <- cbind(ratio_peese_ivols2,ratio_ak_ivols2)
ratio_peese_ak_ivols <- ratio_peese_ak_ivols[,-c(3,4,5)]
ratio_peese_ak_ivols  <-cbind(ratio_peese_ak_ivols[,1:2],ratio_peese_ak_ivols[,4],ratio_peese_ak_ivols[,3],ratio_peese_ak_ivols[,5])
colnames(ratio_peese_ak_ivols) <- c("Meta id", "Ratio-PEESE","Obs (IV)", "Ratio-AK","Obs (OLS)")

ratio_peese_ak_ivols <- data.frame(ratio_peese_ak_ivols)

# print
# Excel file
write.xlsx(ratio_peese_ak_ivols , file = "ratio_peese_ak_ivols.xlsx", sheetName = "Sheet1", rowNames = FALSE,digits=3)
#LaTeX
table_latex_ratio_peese_ak_ivols<- xtable(ratio_peese_ak_ivols , include.rownames = FALSE)
print(table_latex_ratio_peese_ak_ivols ,include.rownames = FALSE)





#PEESE ols vs experimental
for (i in 1:length(peese_ak_olsexp[,4])) {
  ratio_peese_olsexp[i] <- ((peese_ak_olsexp[,3][i]) - (peese_ak_olsexp[,4][i]))/(abs(peese_ak_olsexp[,4][i]))
}
ratio_peese_olsexp <- cbind(peese_ak_olsexp[,1],ratio_peese_olsexp)
# ratio_peese_olsexp[,2] <- as.numeric(round(ratio_peese_olsexp[,2], 3))
ratio_peese_olsexp  <- ratio_peese_olsexp [complete.cases(ratio_peese_olsexp ), ]

#adding the number of observations to each ratio
ratio_peese_olsexp <- cbind(ratio_peese_olsexp,obsols_forexp_final,obsexp_final)
ratio_peese_olsexp[,2] <- as.numeric(round(ratio_peese_olsexp[,2], 3))
colnames(ratio_peese_olsexp) <- c("Meta id", "Ratio", "Obs (OLS)", "Obs (Experimental)")

mean_ratio_peese_olsexp <- mean(ratio_peese_olsexp[,2])
sd_ratio_peese_olsexp <- sd(ratio_peese_olsexp[,2])
mean_ratio_peese_olsexp 
sd_ratio_peese_olsexp 

#AK ols vs experimental
for (i in 1:length(peese_ak_olsexp[,4])) {
  ratio_ak_olsexp[i] <- ((peese_ak_olsexp[,5][i]) - (peese_ak_olsexp[,6][i]))/(abs(peese_ak_olsexp[,6][i]))
}
ratio_ak_olsexp <- cbind(peese_ak_olsexp[,1],ratio_ak_olsexp)
# ratio_ak_olsexp[,2] <- as.numeric(round(ratio_ak_olsexp[,2], 3))
ratio_ak_olsexp  <- ratio_ak_olsexp [complete.cases(ratio_ak_olsexp), ]

#adding the number of observations to each ratio
ratio_ak_olsexp <- cbind(ratio_ak_olsexp,obsols_forexp_final,obsexp_final)
ratio_ak_olsexp[,2] <- as.numeric(round(ratio_ak_olsexp[,2], 3))
colnames(ratio_ak_olsexp) <- c("Meta id", "Ratio", "Obs (OLS)", "Obs (Experimental)")


mean_ratio_ak_olsexp <- mean(ratio_ak_olsexp[,2])
sd_ratio_ak_olsexp <- sd(ratio_ak_olsexp[,2])
mean_ratio_ak_olsexp 
sd_ratio_ak_olsexp 


ratio_peese_ak_olsexp <- cbind(ratio_peese_olsexp,ratio_ak_olsexp)
ratio_peese_ak_olsexp <- ratio_peese_ak_olsexp[,-c(3,4,5)]
ratio_peese_ak_olsexp  <-cbind(ratio_peese_ak_olsexp[,1:2],ratio_peese_ak_olsexp[,4],ratio_peese_ak_olsexp[,3],ratio_peese_ak_olsexp[,5])
colnames(ratio_peese_ak_olsexp) <- c("Meta id", "Ratio-PEESE","Obs (IV)", "Ratio-AK","Obs (OLS)")
ratio_peese_ak_olsexp <- data.frame(ratio_peese_ak_olsexp)
ratio_peese_ak_olsexp <-data.frame(ratio_peese_ak_olsexp)
# print
# Excel file
write.xlsx(ratio_peese_ak_olsexp , file = "ratio_peese_ak_olsexp.xlsx", sheetName = "Sheet1", rowNames = FALSE)
#LaTeX
table_latex_ratio_peese_ak_olsexp<- xtable(ratio_peese_ak_olsexp , include.rownames = FALSE)
print(table_latex_ratio_peese_ak_olsexp ,include.rownames = FALSE)




# Simple means
simple_ivols2 <- simple_ivols[simple_ivols[,1] %in% ratio_ak_ivols2[,1],]
rownames(simple_ivols2) <- NULL
ratio_simple_ivols<-vector()
for (i in 1:length(simple_ivols2[,1])) {
  ratio_simple_ivols[i] <- ((simple_ivols2[,2][i]) - (simple_ivols2[,4][i]))/(abs(simple_ivols2[,4][i]))
}
simple_ivols2[,6] <- ratio_simple_ivols
names(simple_ivols2)[6] <- "Ratio"

lastrow_simple_ivols2 <- c(76,"",sum(simple_ivols2[,3]),"",sum(simple_ivols2[,5]),mean(simple_ivols2[,6]))
lastrow_simple_ivols2 <- as.numeric(lastrow_simple_ivols2)
simple_ivols2 <- rbind(simple_ivols2, lastrow_simple_ivols2)

simple_olsexp2 <- simple_olsexp[simple_olsexp[,1] %in% ratio_ak_olsexp[,1],]
rownames(simple_olsexp2) <- NULL
ratio_simple_olsexp<-vector()
for (i in 1:length(simple_olsexp2[,1])) {
  ratio_simple_olsexp[i] <- ((simple_olsexp2[,2][i]) - (simple_olsexp2[,4][i]))/(abs(simple_olsexp2[,4][i]))
}
simple_olsexp2[,6] <- ratio_simple_olsexp
names(simple_olsexp2)[6] <- "Ratio"

lastrow_simple_olsexp2 <- c(8,"",sum(simple_olsexp2[,3]),"",sum(simple_olsexp2[,5]),mean(simple_olsexp2[,6]))
lastrow_simple_olsexp2 <- as.numeric(lastrow_simple_olsexp2)
simple_olsexp2 <- rbind(simple_olsexp2, lastrow_simple_olsexp2)

# for printing
#iv vs ols
for (i in 1:6) {
  simple_ivols2[,i] <- as.numeric(simple_ivols2[,i])  
}
simple_ivols2 <- simple_ivols2 %>%
  mutate(across(c(2,4,6), ~ format(round(., digits = 3), nsmall = 3)))

simple_ivols2 <- simple_ivols2 %>%
  mutate(across(c(1,3,5), ~ format(round(., digits = 0), nsmall = 0)))

# Excel file
write.xlsx(simple_ivols2 , file = "simple_ivols.xlsx", sheetName = "Sheet1", rowNames = FALSE)
#LaTeX
table_latex_simple_ivols2 <- xtable(simple_ivols2 , include.rownames = FALSE)
print.xtable(table_latex_simple_ivols2 ,include.rownames = FALSE)

# ols vs experimental 

for (i in 1:6) {
  simple_olsexp2[,i] <- as.numeric(simple_olsexp2[,i])  
}
simple_olsexp2 <- simple_olsexp2 %>%
  mutate(across(c(2,4,6), ~ format(round(., digits = 3), nsmall = 3)))

simple_olsexp2 <- simple_olsexp2 %>%
  mutate(across(c(1,3,5), ~ format(round(., digits = 0), nsmall = 0)))

# Excel file
write.xlsx(simple_olsexp2 , file = "simple_olsexp.xlsx", sheetName = "Sheet1", rowNames = FALSE)
#LaTeX
table_latex_simple_olsexp2 <- xtable(simple_olsexp2 , include.rownames = FALSE)
print.xtable(table_latex_simple_olsexp2 ,include.rownames = FALSE)





# Simple medians
simple_median_ivols2 <- simple_median_ivols[simple_median_ivols[,1] %in% ratio_ak_ivols2[,1],]
rownames(simple_median_ivols2) <- NULL
simple_median_ivols2[24,4] <- 0.00005
ratio_simple_median_ivols<-vector()
for (i in 1:length(simple_median_ivols2[,1])) {
  ratio_simple_median_ivols[i] <- ((simple_median_ivols2[,2][i]) - (simple_median_ivols2[,4][i]))/(abs(simple_median_ivols2[,4][i]))
}
simple_median_ivols2[,6] <- ratio_simple_median_ivols
names(simple_median_ivols2)[6] <- "Ratio"

lastrow_simple_median_ivols2 <- c(76,"",sum(simple_median_ivols2[,3]),"",sum(simple_median_ivols2[,5]),mean(simple_median_ivols2[,6]))
lastrow_simple_median_ivols2 <- as.numeric(lastrow_simple_median_ivols2)
simple_median_ivols2 <- rbind(simple_median_ivols2, lastrow_simple_median_ivols2)

simple_median_olsexp2 <- simple_median_olsexp[simple_median_olsexp[,1] %in% ratio_ak_olsexp[,1],]
rownames(simple_median_olsexp2) <- NULL
ratio_simple_median_olsexp<-vector()
for (i in 1:length(simple_median_olsexp2[,1])) {
  ratio_simple_median_olsexp[i] <- ((simple_median_olsexp2[,2][i]) - (simple_median_olsexp2[,4][i]))/(abs(simple_median_olsexp2[,4][i]))
}
simple_median_olsexp2[,6] <- ratio_simple_median_olsexp
names(simple_median_olsexp2)[6] <- "Ratio"

lastrow_simple_median_olsexp2 <- c(8,"",sum(simple_median_olsexp2[,3]),"",sum(simple_median_olsexp2[,5]),mean(simple_median_olsexp2[,6]))
lastrow_simple_median_olsexp2 <- as.numeric(lastrow_simple_median_olsexp2)
simple_median_olsexp2 <- rbind(simple_median_olsexp2, lastrow_simple_median_olsexp2)

# for printing
#iv vs ols
for (i in 1:6) {
  simple_median_ivols2[,i] <- as.numeric(simple_median_ivols2[,i])  
}
simple_median_ivols2 <- simple_median_ivols2 %>%
  mutate(across(c(2,4,6), ~ format(round(., digits = 3), nsmall = 3)))

simple_median_ivols2 <- simple_median_ivols2 %>%
  mutate(across(c(1,3,5), ~ format(round(., digits = 0), nsmall = 0)))

# Excel file
write.xlsx(simple_median_ivols2 , file = "simple_median_ivols.xlsx", sheetName = "Sheet1", rowNames = FALSE)
#LaTeX
table_latex_simple_median_ivols2 <- xtable(simple_median_ivols2 , include.rownames = FALSE)
print.xtable(table_latex_simple_median_ivols2 ,include.rownames = FALSE)

# ols vs experimental 

for (i in 1:6) {
  simple_median_olsexp2[,i] <- as.numeric(simple_median_olsexp2[,i])  
}
simple_median_olsexp2 <- simple_median_olsexp2 %>%
  mutate(across(c(2,4,6), ~ format(round(., digits = 3), nsmall = 3)))

simple_median_olsexp2 <- simple_median_olsexp2 %>%
  mutate(across(c(1,3,5), ~ format(round(., digits = 0), nsmall = 0)))

# Excel file
write.xlsx(simple_median_olsexp2 , file = "simple_median_olsexp.xlsx", sheetName = "Sheet1", rowNames = FALSE)
#LaTeX
table_latex_simple_median_olsexp2 <- xtable(simple_median_olsexp2 , include.rownames = FALSE)
print.xtable(table_latex_simple_median_olsexp2 ,include.rownames = FALSE)





# Final table
ratio_simple_mean_ivols <- mean(as.numeric(simple_ivols2[,6][1:length(simple_ivols2[,6])]))
ratio_simple_sd_ivols <- sd(as.numeric(simple_ivols2[,6][1:length(simple_ivols2[,6])-1]))
ratio_simple_mean_olsexp <- mean(as.numeric(simple_olsexp2[,6][1:length(simple_olsexp2[,6])]))
ratio_simple_sd_olsexp <- sd(as.numeric(simple_olsexp2[,6][1:length(simple_olsexp2[,6])-1]))

ratio_simple_median_mean_ivols <- mean(as.numeric(simple_median_ivols2[,6][1:length(simple_median_ivols2[,6])]))
ratio_simple_median_sd_ivols <- sd(as.numeric(simple_median_ivols2[,6][1:length(simple_median_ivols2[,6])-1]))
ratio_simple_median_mean_olsexp <- mean(as.numeric(simple_median_olsexp2[,6][1:length(simple_median_olsexp2[,6])]))
ratio_simple_median_sd_olsexp <- sd(as.numeric(simple_median_olsexp2[,6][1:length(simple_median_olsexp2[,6])-1]))

ratio_final <- data.frame(
  PEESE = c(mean_ratio_peese_ivols,sd_ratio_peese_ivols,mean_ratio_peese_olsexp,sd_ratio_peese_olsexp),
  AK = c(mean_ratio_ak_ivols,sd_ratio_ak_ivols, mean_ratio_ak_olsexp, sd_ratio_ak_olsexp),
  Simple_mean = c(ratio_simple_mean_ivols,ratio_simple_sd_ivols,ratio_simple_mean_olsexp,ratio_simple_sd_olsexp),
  Simple_median = c(ratio_simple_median_mean_ivols,ratio_simple_median_sd_ivols,
                    ratio_simple_median_mean_olsexp,ratio_simple_median_sd_olsexp)
)
# print
# Excel file
write.xlsx(ratio_final , file = "ratio_final.xlsx", sheetName = "Sheet1", rowNames = FALSE)
#LaTeX
table_latex_ratio_final<- xtable(ratio_final , include.rownames = FALSE)
print(table_latex_ratio_final ,include.rownames = FALSE)




#**---- Ratios (alternative) ----

# empty vectors
ratio2_peese_ivols<-vector()
ratio2_ak_ivols<-vector()
ratio2_peese_olsexp<-vector()
ratio2_ak_olsexp<-vector()

#PEESE iv vs ols

for (i in 1:length(peese_ak_ivols[,4])) {
  ratio2_peese_ivols[i] <- abs((peese_ak_ivols[,4][i])/(peese_ak_ivols[,3][i]))
}
ratio2_peese_ivols <- cbind(peese_ak_ivols[,1],ratio2_peese_ivols)
# ratio2_peese_ivols <- ratio2_peese_ivols[seq(1, nrow(ratio2_peese_ivols), by = 2),]
ratio2_peese_ivols <- ratio2_peese_ivols[complete.cases(ratio2_peese_ivols), ]

#adding the number of observations to each ratio
ratio2_peese_ivols <- cbind(ratio2_peese_ivols,obsiv_final,obsols_foriv_final)
ratio2_peese_ivols[,2] <- as.numeric(round(ratio2_peese_ivols[,2], 3))
colnames(ratio2_peese_ivols) <- c("Meta Id", "Ratio", "Obs (IV)", "Obs (OLS)")

ratio2_peese_ivols2 <-ratio2_peese_ivols[-c(64,69),] #removing outliers
# ratio2_peese_ivols2 <-ratio2_peese_ivols
mean_ratio2_peese_ivols <- mean(ratio2_peese_ivols2[,2])
median_ratio2_peese_ivols <- median(ratio2_peese_ivols2[,2])
sd_ratio2_peese_ivols <- sd(ratio2_peese_ivols2[,2])
mean_ratio2_peese_ivols
median_ratio2_peese_ivols
sd_ratio2_peese_ivols

ratio2_peese_ivols2
sum(ratio2_peese_ivols2[,3])
sum(ratio2_peese_ivols2[,4])


# AK iv vs ols
for (i in 1:length(peese_ak_ivols[,4])) {
  ratio2_ak_ivols[i] <- abs((peese_ak_ivols[,6][i])/(peese_ak_ivols[,5][i]))
}
ratio2_ak_ivols <- cbind(peese_ak_ivols[,1],ratio2_ak_ivols)
ratio2_ak_ivols <- ratio2_ak_ivols[complete.cases(ratio2_ak_ivols), ]

#adding the number of observations to each ratio
ratio2_ak_ivols <- cbind(ratio2_ak_ivols,obsiv_final,obsols_foriv_final)
ratio2_ak_ivols[,2] <- as.numeric(round(ratio2_ak_ivols[,2], 3))
colnames(ratio2_ak_ivols) <- c("Meta id", "Ratio", "Obs (IV)", "Obs (OLS)")

ratio2_ak_ivols2 <-ratio2_ak_ivols[-c(32,64,67,70),] #removing outliers
# ratio2_ak_ivols2 <-ratio2_ak_ivols 
mean_ratio2_ak_ivols <- mean(ratio2_ak_ivols2[,2])
median_ratio2_ak_ivols <- median(ratio2_ak_ivols2[,2])
sd_ratio2_ak_ivols <- sd(ratio2_ak_ivols2[,2])
mean_ratio2_ak_ivols 
median_ratio2_ak_ivols 
sd_ratio2_ak_ivols

ratio2_ak_ivols2
sum(ratio2_ak_ivols2[,3])
sum(ratio2_ak_ivols2[,4])

ratio2_peese_ak_ivols <- cbind(ratio2_peese_ivols,ratio2_ak_ivols)
ratio2_peese_ak_ivols <- ratio2_peese_ak_ivols[,-c(3,4,5)]
ratio2_peese_ak_ivols  <-cbind(ratio2_peese_ak_ivols[,1:2],ratio2_peese_ak_ivols[,4],ratio2_peese_ak_ivols[,3],ratio2_peese_ak_ivols[,5])
colnames(ratio2_peese_ak_ivols) <- c("Meta id", "Ratio-PEESE","Obs (IV)", "Ratio-AK","Obs (OLS)")

ratio2_peese_ak_ivols <- data.frame(ratio2_peese_ak_ivols)

# print
# Excel file
write.xlsx(ratio2_peese_ak_ivols , file = "ratio2_peese_ak_ivols.xlsx", sheetName = "Sheet1", rowNames = FALSE,digits=3)
#LaTeX
table_latex_ratio2_peese_ak_ivols<- xtable(ratio2_peese_ak_ivols , include.rownames = FALSE)
print(table_latex_ratio2_peese_ak_ivols ,include.rownames = FALSE)





#PEESE ols vs experimental
for (i in 1:length(peese_ak_olsexp[,4])) {
  ratio2_peese_olsexp[i] <- abs((peese_ak_olsexp[,3][i])/(peese_ak_olsexp[,4][i]))
}
ratio2_peese_olsexp <- cbind(peese_ak_olsexp[,1],ratio2_peese_olsexp)
# ratio2_peese_olsexp[,2] <- as.numeric(round(ratio2_peese_olsexp[,2], 3))
ratio2_peese_olsexp  <- ratio2_peese_olsexp [complete.cases(ratio2_peese_olsexp ), ]

#adding the number of observations to each ratio
ratio2_peese_olsexp <- cbind(ratio2_peese_olsexp,obsols_forexp_final,obsexp_final)
ratio2_peese_olsexp[,2] <- as.numeric(round(ratio2_peese_olsexp[,2], 3))
colnames(ratio2_peese_olsexp) <- c("Meta id", "Ratio", "Obs (OLS)", "Obs (Experimental)")

mean_ratio2_peese_olsexp <- mean(ratio2_peese_olsexp[,2])
sd_ratio2_peese_olsexp <- sd(ratio2_peese_olsexp[,2])
mean_ratio2_peese_olsexp 
sd_ratio2_peese_olsexp 

#AK ols vs experimental
for (i in 1:length(peese_ak_olsexp[,4])) {
  ratio2_ak_olsexp[i] <- abs((peese_ak_olsexp[,5][i])/(peese_ak_olsexp[,6][i]))
}
ratio2_ak_olsexp <- cbind(peese_ak_olsexp[,1],ratio2_ak_olsexp)
# ratio2_ak_olsexp[,2] <- as.numeric(round(ratio2_ak_olsexp[,2], 3))
ratio2_ak_olsexp  <- ratio2_ak_olsexp [complete.cases(ratio2_ak_olsexp), ]

#adding the number of observations to each ratio
ratio2_ak_olsexp <- cbind(ratio2_ak_olsexp,obsols_forexp_final,obsexp_final)
ratio2_ak_olsexp[,2] <- as.numeric(round(ratio2_ak_olsexp[,2], 3))
colnames(ratio2_ak_olsexp) <- c("Meta id", "Ratio", "Obs (OLS)", "Obs (Experimental)")


mean_ratio2_ak_olsexp <- mean(ratio2_ak_olsexp[,2])
sd_ratio2_ak_olsexp <- sd(ratio2_ak_olsexp[,2])
mean_ratio2_ak_olsexp 
sd_ratio2_ak_olsexp 


ratio2_peese_ak_olsexp <- cbind(ratio2_peese_olsexp,ratio2_ak_olsexp)
ratio2_peese_ak_olsexp <- ratio2_peese_ak_olsexp[,-c(3,4,5)]
ratio2_peese_ak_olsexp  <-cbind(ratio2_peese_ak_olsexp[,1:2],ratio2_peese_ak_olsexp[,4],ratio2_peese_ak_olsexp[,3],ratio2_peese_ak_olsexp[,5])
colnames(ratio2_peese_ak_olsexp) <- c("Meta id", "Ratio-PEESE","Obs (IV)", "Ratio-AK","Obs (OLS)")
ratio2_peese_ak_olsexp <- data.frame(ratio2_peese_ak_olsexp)
ratio2_peese_ak_olsexp <-data.frame(ratio2_peese_ak_olsexp)
# print
# Excel file
write.xlsx(ratio2_peese_ak_olsexp , file = "ratio2_peese_ak_olsexp.xlsx", sheetName = "Sheet1", rowNames = FALSE)
#LaTeX
table_latex_ratio2_peese_ak_olsexp<- xtable(ratio2_peese_ak_olsexp , include.rownames = FALSE)
print(table_latex_ratio2_peese_ak_olsexp ,include.rownames = FALSE)




# Simple means
#IV vs OLS
simple2_ivols2 <- simple_ivols[simple_ivols[,1] %in% ratio2_ak_ivols2[,1],]
rownames(simple2_ivols2) <- NULL
ratio2_simple2_ivols<-vector()
for (i in 1:length(simple2_ivols2[,1])) {
  ratio2_simple2_ivols[i] <- abs((simple2_ivols2[,2][i])/(simple2_ivols2[,4][i]))
}
simple2_ivols2[,6] <- ratio2_simple2_ivols
names(simple2_ivols2)[6] <- "Ratio"

lastrow_simple2_ivols2 <- c(length(simple2_ivols2[,1]),"",sum(simple2_ivols2[,3]),"",sum(simple2_ivols2[,5]),mean(simple2_ivols2[,6]))
lastrow_simple2_ivols2 <- as.numeric(lastrow_simple2_ivols2)
simple2_ivols2 <- rbind(simple2_ivols2, lastrow_simple2_ivols2)

mean(simple2_ivols2[,6])
median(simple2_ivols2[,6])
sd(simple2_ivols2[,6])


# Extra studies included
simple2_ivols2 <- simple_ivols[simple_ivols[,1] %in% ratio2_ak_ivols2[,1],]
simple2_ivols2 <- simple_ivols
rownames(simple2_ivols2) <- NULL
ratio2_simple2_ivols<-vector()
for (i in 1:length(simple2_ivols2[,1])) {
  ratio2_simple2_ivols[i] <- abs((simple2_ivols2[,4][i])/(simple2_ivols2[,2][i]))
}
simple2_ivols2[,6] <- ratio2_simple2_ivols
names(simple2_ivols2)[6] <- "Ratio"

mean(simple2_ivols2[,6][-c(47,82)])
median(simple2_ivols2[,6][-c(47,82)])
sd(simple2_ivols2[,6][-c(47,82)])


lastrow_simple2_ivols2 <- c(length(simple2_ivols2[,1]),"",sum(simple2_ivols2[,3][-c(47,82)]),"",sum(simple2_ivols2[,5][-c(47,82)]),median(simple2_ivols2[,6][-c(47,82)]))
lastrow_simple2_ivols2 <- as.numeric(lastrow_simple2_ivols2)
simple2_ivols2 <- rbind(simple2_ivols2, lastrow_simple2_ivols2)
simple2_ivols2


#OLS vs Experimental
simple2_olsexp2 <- simple_olsexp[simple_olsexp[,1] %in% ratio2_ak_olsexp[,1],]
rownames(simple2_olsexp2) <- NULL
ratio2_simple2_olsexp<-vector()
for (i in 1:length(simple2_olsexp2[,1])) {
  ratio2_simple2_olsexp[i] <- abs((simple2_olsexp2[,2][i])/(simple2_olsexp2[,4][i]))
}
simple2_olsexp2[,6] <- ratio2_simple2_olsexp
names(simple2_olsexp2)[6] <- "Ratio"

lastrow_simple2_olsexp2 <- c(8,"",sum(simple2_olsexp2[,3]),"",sum(simple2_olsexp2[,5]),mean(simple2_olsexp2[,6]))
lastrow_simple2_olsexp2 <- as.numeric(lastrow_simple2_olsexp2)
simple2_olsexp2 <- rbind(simple2_olsexp2, lastrow_simple2_olsexp2)

# for printing
#iv vs ols
for (i in 1:6) {
  simple2_ivols2[,i] <- as.numeric(simple2_ivols2[,i])  
}
simple2_ivols2 <- simple2_ivols2 %>%
  mutate(across(c(2,4,6), ~ format(round(., digits = 3), nsmall = 3)))

simple2_ivols2 <- simple2_ivols2 %>%
  mutate(across(c(1,3,5), ~ format(round(., digits = 0), nsmall = 0)))

# Excel file
write.xlsx(simple2_ivols2 , file = "simple2_ivols.xlsx", sheetName = "Sheet1", rowNames = FALSE)
#LaTeX
table_latex_simple2_ivols2 <- xtable(simple2_ivols2 , include.rownames = FALSE)
print.xtable(table_latex_simple2_ivols2 ,include.rownames = FALSE)

# ols vs experimental 

for (i in 1:6) {
  simple2_olsexp2[,i] <- as.numeric(simple2_olsexp2[,i])  
}
simple2_olsexp2 <- simple2_olsexp2 %>%
  mutate(across(c(2,4,6), ~ format(round(., digits = 3), nsmall = 3)))

simple2_olsexp2 <- simple2_olsexp2 %>%
  mutate(across(c(1,3,5), ~ format(round(., digits = 0), nsmall = 0)))

# Excel file
write.xlsx(simple2_olsexp2 , file = "simple2_olsexp.xlsx", sheetName = "Sheet1", rowNames = FALSE)
#LaTeX
table_latex_simple2_olsexp2 <- xtable(simple2_olsexp2 , include.rownames = FALSE)
print.xtable(table_latex_simple2_olsexp2 ,include.rownames = FALSE)





# Simple medians
simple2_median_ivols2 <- simple_median_ivols[simple_median_ivols[,1] %in% ratio2_ak_ivols2[,1],]
rownames(simple2_median_ivols2) <- NULL
simple2_median_ivols2[24,4] <- 0.0005
simple2_median_ivols2[25,4] <- 0.005
ratio2_simple2_median_ivols<-vector()
for (i in 1:length(simple2_median_ivols2[,1])) {
  ratio2_simple2_median_ivols[i] <- abs((simple2_median_ivols2[,2][i])/(simple2_median_ivols2[,4][i]))
}
simple2_median_ivols2[,6] <- ratio2_simple2_median_ivols
names(simple2_median_ivols2)[6] <- "Ratio"

lastrow_simple2_median_ivols2 <- c(76,"",sum(simple2_median_ivols2[,3]),"",sum(simple2_median_ivols2[,5]),median(simple2_median_ivols2[,6]))
lastrow_simple2_median_ivols2 <- as.numeric(lastrow_simple2_median_ivols2)

simple2_median_ivols2
median(simple2_median_ivols2[,6])
sd(simple2_median_ivols2[,6])

# Extra studies included
simple2_median_ivols2 <- simple_median_ivols[simple_median_ivols[,1] %in% ratio2_ak_ivols2[,1],]
simple2_median_ivols2 <- simple_median_ivols
rownames(simple2_median_ivols2) <- NULL
# simple2_median_ivols2[24,4] <- 0.0005
# simple2_median_ivols2[25,4] <- 0.005
ratio2_simple2_median_ivols<-vector()
for (i in 1:length(simple2_median_ivols2[,1])) {
  ratio2_simple2_median_ivols[i] <- abs((simple2_median_ivols2[,4][i])/(simple2_median_ivols2[,2][i]))
}
simple2_median_ivols2[,6] <- ratio2_simple2_median_ivols
names(simple2_median_ivols2)[6] <- "Ratio"


simple2_median_ivols2
median(simple2_median_ivols2[,6][-c(47,82)])
sd(simple2_median_ivols2[,6][-c(47,82)])

lastrow_simple2_median_ivols2 <- c(length(simple2_median_ivols2[,1]),"",sum(simple2_median_ivols2[,3][-82]),"",sum(simple2_median_ivols2[,5][-82]),median(simple2_median_ivols2[,6][-82]))
lastrow_simple2_median_ivols2 <- as.numeric(lastrow_simple2_median_ivols2)

simple2_median_ivols2 <- rbind(simple2_median_ivols2, lastrow_simple2_median_ivols2)
simple2_median_ivols2


simple2_median_olsexp2 <- simple_median_olsexp[simple_median_olsexp[,1] %in% ratio2_ak_olsexp[,1],]
rownames(simple2_median_olsexp2) <- NULL
ratio2_simple2_median_olsexp<-vector()
for (i in 1:length(simple2_median_olsexp2[,1])) {
  ratio2_simple2_median_olsexp[i] <- abs((simple2_median_olsexp2[,2][i])/(simple2_median_olsexp2[,4][i]))
}
simple2_median_olsexp2[,6] <- ratio2_simple2_median_olsexp
names(simple2_median_olsexp2)[6] <- "Ratio"

lastrow_simple2_median_olsexp2 <- c(8,"",sum(simple2_median_olsexp2[,3]),"",sum(simple2_median_olsexp2[,5]),mean(simple2_median_olsexp2[,6]))
lastrow_simple2_median_olsexp2 <- as.numeric(lastrow_simple2_median_olsexp2)
simple2_median_olsexp2 <- rbind(simple2_median_olsexp2, lastrow_simple2_median_olsexp2)

median(simple2_median_ivols2[,6])
sd(simple2_median_ivols2[,6])

# for printing
#iv vs ols
for (i in 1:6) {
  simple2_median_ivols2[,i] <- as.numeric(simple2_median_ivols2[,i])  
}
simple2_median_ivols2 <- simple2_median_ivols2 %>%
  mutate(across(c(2,4,6), ~ format(round(., digits = 3), nsmall = 3)))

simple2_median_ivols2 <- simple2_median_ivols2 %>%
  mutate(across(c(1,3,5), ~ format(round(., digits = 0), nsmall = 0)))

# Excel file
write.xlsx(simple2_median_ivols2 , file = "simple2_median_ivols.xlsx", sheetName = "Sheet1", rowNames = FALSE)
#LaTeX
table_latex_simple2_median_ivols2 <- xtable(simple2_median_ivols2 , include.rownames = FALSE)
print.xtable(table_latex_simple2_median_ivols2 ,include.rownames = FALSE)

# ols vs experimental 

for (i in 1:6) {
  simple2_median_olsexp2[,i] <- as.numeric(simple2_median_olsexp2[,i])  
}
simple2_median_olsexp2 <- simple2_median_olsexp2 %>%
  mutate(across(c(2,4,6), ~ format(round(., digits = 3), nsmall = 3)))

simple2_median_olsexp2 <- simple2_median_olsexp2 %>%
  mutate(across(c(1,3,5), ~ format(round(., digits = 0), nsmall = 0)))

# Excel file
write.xlsx(simple2_median_olsexp2 , file = "simple2_median_olsexp.xlsx", sheetName = "Sheet1", rowNames = FALSE)
#LaTeX
table_latex_simple2_median_olsexp2 <- xtable(simple2_median_olsexp2 , include.rownames = FALSE)
print.xtable(table_latex_simple2_median_olsexp2 ,include.rownames = FALSE)





# Final table
ratio2_simple2_mean_ivols <- mean(as.numeric(simple2_ivols2[,6][1:length(simple2_ivols2[,6])]))
ratio2_simple2_sd_ivols <- sd(as.numeric(simple2_ivols2[,6][1:length(simple2_ivols2[,6])-1]))
ratio2_simple2_mean_olsexp <- mean(as.numeric(simple2_olsexp2[,6][1:length(simple2_olsexp2[,6])]))
ratio2_simple2_sd_olsexp <- sd(as.numeric(simple2_olsexp2[,6][1:length(simple2_olsexp2[,6])-1]))

ratio2_simple2_median_mean_ivols <- mean(as.numeric(simple2_median_ivols2[,6][1:length(simple2_median_ivols2[,6])]))
ratio2_simple2_median_sd_ivols <- sd(as.numeric(simple2_median_ivols2[,6][1:length(simple2_median_ivols2[,6])-1]))
ratio2_simple2_median_mean_olsexp <- mean(as.numeric(simple2_median_olsexp2[,6][1:length(simple2_median_olsexp2[,6])]))
ratio2_simple2_median_sd_olsexp <- sd(as.numeric(simple2_median_olsexp2[,6][1:length(simple2_median_olsexp2[,6])-1]))

ratio2_final <- data.frame(
  PEESE = c(mean_ratio2_peese_ivols,sd_ratio2_peese_ivols,mean_ratio2_peese_olsexp,sd_ratio2_peese_olsexp),
  AK = c(mean_ratio2_ak_ivols,sd_ratio2_ak_ivols, mean_ratio2_ak_olsexp, sd_ratio2_ak_olsexp),
  Simple_mean = c(ratio2_simple2_mean_ivols,ratio2_simple2_sd_ivols,ratio2_simple2_mean_olsexp,ratio2_simple2_sd_olsexp),
  Simple_median = c(ratio2_simple2_median_mean_ivols,ratio2_simple2_median_sd_ivols,
                    ratio2_simple2_median_mean_olsexp,ratio2_simple2_median_sd_olsexp)
)
# print
# Excel file
write.xlsx(ratio2_final , file = "ratio2_final.xlsx", sheetName = "Sheet1", rowNames = FALSE)
#LaTeX
table_latex_ratio2_final<- xtable(ratio2_final , include.rownames = FALSE)
print(table_latex_ratio2_final ,include.rownames = FALSE)





#**---- Plots ------

original_means_iv  <- simple_ivols2[,2]
original_means_iv <- as.numeric(original_means_iv[-76])
original_means_ols <- simple_ivols2[,4]
original_means_ols <- as.numeric(original_means_ols[-76])

original_means_iv <- original_means_iv[original_means_iv<5]
original_means_ols <-as.numeric(original_means_ols[original_means_ols < 5])


indicis <- seq(1,231,3)  
corrected_peese_iv  <- as.numeric(peese_ak_ivols[,3][indicis])
corrected_peese_ols <- as.numeric(peese_ak_ivols[,4][indicis])
corrected_peese_iv  <- corrected_peese_iv[-c(4,44)]
corrected_peese_ols <- corrected_peese_ols[-c(4,44)]

corrected_peese_iv <- corrected_peese_iv[corrected_peese_iv < 5]
corrected_peese_ols <- as.numeric(corrected_peese_ols[corrected_peese_ols < 5])
 
indicis <- seq(1,231,3)  
corrected_ak_iv  <- as.numeric(peese_ak_ivols[,5][indicis])
corrected_ak_ols <- as.numeric(peese_ak_ivols[,6][indicis])
corrected_ak_iv  <- corrected_ak_iv[-c(4,44)]
corrected_ak_ols <- corrected_ak_ols[-c(4,44)]

corrected_ak_iv <- corrected_ak_iv[corrected_ak_iv < 5]
corrected_ak_ols <- as.numeric(corrected_ak_ols[corrected_ak_ols < 5])  



# PEESE IV
pdf("peese_iv.pdf")
hist(original_means_iv,main= "PEESE", xlab="Mean effects",col=rgb(1,0,0,0.5), xlim = c(-1.5,1.5), ylim = c(0,40),
     breaks = 96)
hist(corrected_peese_iv,col=rgb(0,0,1,0.5), xlim = c(-1.5,1.5),ylim = c(0,40),
     breaks = 10,add=TRUE)
#add legend
legend('topright', c('Mean IV estimates', "Corrected IV \nmean estimates"), cex=1.2,fill=c(col=rgb(1,0,0,0.5), col=rgb(0,0,1,0.5))
       ,bty = "n")
dev.off()

# PEESE OLS
pdf("peese_ols.pdf")
hist(original_means_ols,main= "PEESE",xlab = "Mean effects",col=rgb(1,0,0,0.5), xlim = c(-1.5,1.5), ylim = c(0,40),
     breaks = 25)
hist(corrected_peese_ols,col=rgb(0,0,1,0.5), xlim = c(-1.5,1.5), ylim = c(0,40),
     breaks = 16, add=TRUE)
#add legend
legend('topright', c('Mean OLS estimates', "Corrected OLS \nmean estimates"), cex=1.2,fill=c(col=rgb(1,0,0,0.5), col=rgb(0,0,1,0.5))
       ,bty = "n")
dev.off()

# AK IV
pdf("ak_iv.pdf")
hist(original_means_iv,main= "AK 2019",xlab="Mean effects",col=rgb(1,0,0,0.5), xlim = c(-1.5,1.5), ylim = c(0,40),
     breaks = 76)
hist(corrected_ak_iv,col=rgb(0,0,1,0.5), xlim = c(-1.5,1.5),ylim = c(0,40),
     breaks = 16,add=TRUE)
#add legend
legend('topright', c('Mean IV estimates', "Corrected IV \nmean estimates"), cex=1.2,fill=c(col=rgb(1,0,0,0.5), col=rgb(0,0,1,0.5))
       ,bty = "n")

dev.off()


# AK OLS
pdf("ak_ols.pdf")
hist(original_means_ols,main= "AK 2019",xlab = "Mean effects",col=rgb(1,0,0,0.5), xlim = c(-1.5,1.5),ylim = c(0,40),
     breaks = 26)
hist(corrected_ak_ols,col=rgb(0,0,1,0.5), xlim = c(-1.5,1.5), ylim = c(0,40),
     breaks = 14,add=TRUE)
#add legend
legend('topright', c('Mean OLS estimates', "Corrected OLS \nmean estimates"), cex=1.2,fill=c(col=rgb(1,0,0,0.5), col=rgb(0,0,1,0.5))
       ,bty = "n")

dev.off()






##### Robust Bayesian Model Averaging RoBMA #####
# library("RoBMA")
# library("rjags")
# 
# df_robma <- df_ols[df_ols$metaid == 78,]
# length(df_robma[,1])
# fit <- RoBMA(d = df_robma$effect, se = df_robma$se
#              # ,model_type = "2w"
#              )
# fit
# summary(fit)
# plot(fit, parameter = "mu")
# forest(fit)
# plot_models(fit)



# lkl<-TRUE
# lkl<-FALSE
# 
# if (lkl) {
#   
#   ooo<-4+6
#   
# }  else{
#     ooo<-55
#   
# }
