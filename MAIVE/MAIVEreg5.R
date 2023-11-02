library("tidyverse") # This will load ggplot2, dplyr, readxl, and other essential packages
library("writexl")
library("readxl")

rm(list = ls())
# Set Working Directory
setwd("H:/My Drive/BIAS/bias/MAIVE")

# DATA ####
# Data Import
general  <- read_excel("H:/My Drive/BIAS/DATA/MAIVE.xlsx") 

# Renaming and Calculating within a Single Pipe
df <- general %>%
  rename_with(~c("bs", "sebs", "Ns", "studyid", "meta_id", "ncoefm"), 
              .cols = c("E5", "SE5", "N5", "studyID", "metaID", "n_E_m"))
# group_by(meta_id) %>%
# mutate(onecoefm = sum(n_E_s == 1)/n_study_m) %>%  #onecoefm = sum(n_E_s == 1)/ncoefm
# ungroup() 
#all 
df   <- df %>% select(bs, sebs, Ns, studyid, meta_id) 
df <- split(df, df$meta_id)
df <- lapply(df, function(df) {
  df$meta_id <- NULL
  return(df)
})
names(df) <- paste0("Meta_", names(df))

#published   
dfp   <- df[df$studyPublishD == 1,              ] %>% select(bs, sebs, Ns, studyid, meta_id) 
dfp <- split(dfp, dfp$meta_id)
dfp <- lapply(dfp, function(dfp) {
  dfp$meta_id <- NULL
  return(dfp)
})
names(dfp) <- paste0("Meta_", names(dfp))

#not published    
dfwp  <- df[df$studyPublishD == 0,              ] %>% select(bs, sebs, Ns, studyid, meta_id) 
dfwp <- split(dfwp, dfwp$meta_id)
dfwp <- lapply(dfwp, function(dfwp) {
  dfwp$meta_id <- NULL
  return(dfwp)
})
names(dfwp) <- paste0("Meta_", names(dfwp))

# not published, new 
dfwps <- df[df$studyPublishD == 0 & df$np == 0, ] %>% select(bs, sebs, Ns, studyid, meta_id) 
dfwps <- split(dfwps, dfwps$meta_id)
dfwps <- lapply(dfwps, function(dfwps) {
  dfwps$meta_id <- NULL
  return(dfwps)
})
names(dfwps) <- paste0("Meta_", names(dfwps))

# not published, old
dfnp  <- df[df$studyPublishD == 0 & df$np == 1, ] %>% select(bs, sebs, Ns, studyid, meta_id) 
dfnp <- split(dfnp, dfnp$meta_id)
dfnp <- lapply(dfnp, function(dfnp) {
  dfnp$meta_id <- NULL
  return(dfnp)
})
names(dfnp) <- paste0("Meta_", names(dfnp))


# Unique meta_ids
numbers <- df$meta_id %>% unique()

# FIXED EFFECT REGRESSION ####
## Choose the subset ####
# y <- "all"
# object<-c("E","SE","F","Hausman","CV_of_Chi2", "Obs")
# list_of_meta <- df

# y <- "p"
# object<-c("pE","pSE","pF","pHausman","pCV" , "pObs")
# list_of_meta <- dfp

# y <- "wp"
# object<-c("wpE","wpSE","wpF","wpHausman","wpCV" ,"wpObs")
# list_of_meta <- dfwp
# 
# y <- "wps"
# object<-c("wpsE","wpsSE","wpsF","wpsHausman","wpsCV", "wpsObs")
# list_of_meta <- dfwps

# y <- "np"
# object<-c("npE","npSE","npF","npHausman","npCV" , "npObs")
# list_of_meta <- dfnp


## Fixed Effects ####
source("maivebiasFE.R")
x <- "FE"

results <- as.data.frame(t(MAIVEresults))
colnames(results) <- results[1, ]
results <- results[-1, ] %>%
  mutate(across(everything(), as.numeric))

# Add row names as a new column in the data frame
results_with_row_names <- results %>% 
  tibble::add_column(Row_Names = row.names(results), .before = 1)

# Save the modified data frame to an Excel file
write_xlsx(results_with_row_names, paste0("MAIVE5_", y, x , ".xlsx"))

# BETWEEN EFFECT REGRESSION ####
## DATA ####

averages <- general %>%
  group_by(metaID, studyID) %>%
  summarise(bs = mean(E5, na.rm = TRUE),
            sebs = mean(SE5, na.rm = TRUE),
            Ns = mean(N5, na.rm = TRUE),
            studyPublishD = mean(studyPublishD),
            np = mean(np)
  )

data <- averages %>% rename_with(~c("meta_id", "studyid"),.cols = c("metaID", "studyID"))


#all 
df <- data %>% select(bs, sebs, Ns, studyid, meta_id) 
df <- split(df, df$meta_id)
df <- lapply(df, function(df) {
  df$meta_id <- NULL
  return(df)
})
names(df) <- paste0("Meta_", names(df))

#published   
dfp   <- data[data$studyPublishD == 1,              ] %>% select(bs, sebs, Ns, studyid, meta_id) 
dfp <- split(dfp, dfp$meta_id)
dfp <- lapply(dfp, function(dfp) {
  dfp$meta_id <- NULL
  return(dfp)
})
names(dfp) <- paste0("Meta_", names(dfp))

#not published    
dfwp  <- data[data$studyPublishD == 0,              ] %>% select(bs, sebs, Ns, studyid, meta_id) 
dfwp <- split(dfwp, dfwp$meta_id)
dfwp <- lapply(dfwp, function(dfwp) {
  dfwp$meta_id <- NULL
  return(dfwp)
})
names(dfwp) <- paste0("Meta_", names(dfwp))

# not published, new 
dfwps <- data[data$studyPublishD == 0 & data$np == 0, ] %>% select(bs, sebs, Ns, studyid, meta_id) 
dfwps <- split(dfwps, dfwps$meta_id)
dfwps <- lapply(dfwps, function(dfwps) {
  dfwps$meta_id <- NULL
  return(dfwps)
})
names(dfwps) <- paste0("Meta_", names(dfwps))

# not published, old
dfnp  <- data[data$studyPublishD == 0 & data$np == 1, ] %>% select(bs, sebs, Ns, studyid, meta_id) 
dfnp <- split(dfnp, dfnp$meta_id)
dfnp <- lapply(dfnp, function(dfnp) {
  dfnp$meta_id <- NULL
  return(dfnp)
})
names(dfnp) <- paste0("Meta_", names(dfnp))


# Unique meta_ids
numbers <- data$meta_id %>% unique()



## Choose the subset ####
# y <- "all"
# object<-c("E","SE","F","Hausman","CV_of_Chi2", "Obs")
# list_of_meta <- df

# y <- "p"
# object<-c("pE","pSE","pF","pHausman","pCV" , "pObs")
# list_of_meta <- dfp

# y <- "wp"
# object<-c("wpE","wpSE","wpF","wpHausman","wpCV" ,"wpObs")
# list_of_meta <- dfwp

# y <- "wps"
# object<-c("wpsE","wpsSE","wpsF","wpsHausman","wpsCV", "wpsObs")
# list_of_meta <- dfwps

# y <- "np"
# object<-c("npE","npSE","npF","npHausman","npCV" , "npObs")
# list_of_meta <- dfnp


## Fixed Effects ####
source("maivebias.R")
x <- "BE"

results <- as.data.frame(t(MAIVEresults))
colnames(results) <- results[1, ]
results <- results[-1, ] %>%
  mutate(across(everything(), as.numeric))

# Add row names as a new column in the data frame
results_with_row_names <- results %>% 
  tibble::add_column(Row_Names = row.names(results), .before = 1)

# Save the modified data frame to an Excel file
write_xlsx(results_with_row_names, paste0("MAIVE5_", y, x , ".xlsx"))



