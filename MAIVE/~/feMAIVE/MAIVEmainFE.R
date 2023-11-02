library("tidyverse")
library("ggplot2")
library("scales")
library("readxl")
library("cowplot") 
library("patchwork") #putting plots together 
library("dplyr")
library("writexl")
library("robustHD")
library("stringr")
rm(list = ls())
setwd("H:/My Drive/~P-Hacking/0.1.MAIVE")
# Import df ####
data  <- read_excel("H:/My Drive/~P-Hacking/Data/GeneralDataworkwinsorized.xlsx", sheet ="Sheet1" ) 

df <- data %>%
  rename(
    bs       = "coeff_2",
    sebs     = "stdev_2",
    Ns       = "sample_2",
    study_id = "studyID",
    meta_id  = "metaID", 
    ncoefm   = "n_coeff_m") %>%
  group_by(meta_id) %>%
  mutate(onecoef = sum(n_coeff_s == 1)/ncoefm  )  %>% 
  ungroup()  

numbers <-  unique(df$meta_id)

# ALL DATA ####
all <- df %>% 
  select(bs, sebs, Ns, study_id, meta_id, onecoef, ncoefm)

for (i in numbers){
  assign(paste0("Meta_", i), subset(all, meta_id == i))
}
object<-c("coef","se","F-test","Hausman","CV of Chi2(1)" ,"metaID", "obs")
source("maivebiasFE.R")

# Save the results 
allMAIVE <-as.data.frame(t(MAIVEresults))
colnames(allMAIVE) <- allMAIVE[1,]
allMAIVE <- allMAIVE[-1,]
allMAIVE[] <- lapply(allMAIVE, as.numeric)

# Save to Excel using openxlsx
# write_xlsx(allMAIVE, "MAIVEall.xlsx")

#clean up 
for (i in numbers) {
  var_name <- paste0("Meta_", i)
  if (exists(var_name)) {
    rm(list = var_name)
  }
} 
rm(all)
# Published ####
p <- df[df$studyPublishD == 1, ] %>% 
  select(bs, sebs, Ns, study_id, meta_id, onecoef, ncoefm)
for (i in numbers){
  assign(paste0("Meta_", i), subset(p, meta_id == i))
}

object<-c("p_coef","p_se","p_F-test","p_Hausman","p_CV of Chi2(1)" ,"metaID", "p_obs")
source("maivebiasFE.R")

# Save the results 
pMAIVE <-as.data.frame(t(MAIVEresults))
colnames(pMAIVE) <- pMAIVE[1,]
pMAIVE <- pMAIVE[-1,]
pMAIVE[] <- lapply(pMAIVE, as.numeric)

# Save to Excel using openxlsx
# write_xlsx(pMAIVE, "MAIVEp.xlsx")

#clean up 
for (i in numbers) {
  var_name <- paste0("Meta_", i)
  if (exists(var_name)) {
    rm(list = var_name)
  }
} 
rm(p)
# Never Published  ####
np <- df[df$np == 1 & !is.na(df$np), ] %>% 
  select(bs, sebs, Ns, study_id, meta_id, onecoef, ncoefm)
for (i in numbers){
  assign(paste0("Meta_", i), subset(np, meta_id == i))
}

object<-c("np_coef","np_se","np_F-test","np_Hausman","np_CV of Chi2(1)" ,"metaID", "np_obs")
source("maivebiasFE.R")

# Save the results 
npMAIVE <-as.data.frame(t(MAIVEresults))
colnames(npMAIVE) <- npMAIVE[1,]
npMAIVE <- npMAIVE[-1,]
npMAIVE[] <- lapply(npMAIVE, as.numeric)

# Save to Excel using openxlsx
# write_xlsx(npMAIVE, "MAIVEnp.xlsx")

# clean up 
for (i in numbers) {
  var_name <- paste0("Meta_", i)
  if (exists(var_name)) {
    rm(list = var_name)
  }
} 
rm(np)

# working papers  ####
wp <- df[df$studyPublishD == 0, ] %>% 
  select(bs, sebs, Ns, study_id, meta_id, onecoef, ncoefm)
for (i in numbers){
  assign(paste0("Meta_", i), subset(wp, meta_id == i))
}

object<-c("wp_coef","wp_se","wp_F-test","wp_Hausman","wp_CV of Chi2(1)" ,"metaID", "wp_obs")
source("maivebiasFE.R")

# Save the results 
wpMAIVE <-as.data.frame(t(MAIVEresults))
colnames(wpMAIVE) <- wpMAIVE[1,]
wpMAIVE <- wpMAIVE[-1,]
wpMAIVE[] <- lapply(wpMAIVE, as.numeric)
# Save to Excel using openxlsx
# write_xlsx(wpMAIVE, "MAIVEwp.xlsx")

#clean up 
for (i in numbers) {
  var_name <- paste0("Meta_", i)
  if (exists(var_name)) {
    rm(list = var_name)
  }
} 
rm(wp)
# working papers without np ####
wps <- df[df$studyPublishD == 0 & df$np == 0, ] %>% 
  select(bs, sebs, Ns, study_id, meta_id, onecoef, ncoefm)
for (i in numbers){
  assign(paste0("Meta_", i), subset(wps, meta_id == i))
}

object<-c("wps_coef","wps_se","wps_F-test","wps_Hausman","wps_CV of Chi2(1)" ,"metaID", "wps_obs")
source("maivebiasFE.R")

# Save the results 
wpsMAIVE <-as.data.frame(t(MAIVEresults))
colnames(wpsMAIVE) <- wpsMAIVE[1,]
wpsMAIVE <- wpsMAIVE[-1,]
wpsMAIVE[] <- lapply(wpsMAIVE, as.numeric)

# Save to Excel using openxlsx
# write_xlsx(wpsMAIVE, "MAIVEwps.xlsx")

#clean up 
for (i in numbers) {
  var_name <- paste0("Meta_", i)
  if (exists(var_name)) {
    rm(list = var_name)
  }
} 
rm(wps)
#MERGE ####

a <- merge(allMAIVE, pMAIVE, by="metaID", all.x=TRUE, all.y=TRUE)
a <- merge(a, npMAIVE, by="metaID", all.x=TRUE, all.y=TRUE)
b <- merge(wpMAIVE, wpsMAIVE, by="metaID", all.x=TRUE, all.y=TRUE)
merge <- merge(a, b, by="metaID", all.x=TRUE, all.y=TRUE)
rm(a,b, dat)
write_xlsx(merge, "MAIVEresultsFE_2N.xlsx")