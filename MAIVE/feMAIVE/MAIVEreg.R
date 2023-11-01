library("tidyverse") # This will load ggplot2, dplyr, readxl, and other essential packages
library("writexl")
library("readxl")

rm(list = ls())
# Set Working Directory
setwd("H:/My Drive/BIAS/bias/MAIVE/feMAIVE")

# Data Import
general  <- read_excel("H:/My Drive/BIAS/DATA/MAIVE.xlsx") 

# Renaming and Calculating within a Single Pipe
df <- general %>%
  rename_with(~c("bs", "sebs", "Ns", "studyid", "meta_id", "ncoefm"), .cols = c("E2", "SE2", "N2", "studyID", "metaID", "n_E_m")) %>%
  group_by(meta_id) %>%
  mutate(#onecoefm = sum(n_E_s == 1)/ncoefm
         onecoefm = sum(n_E_s == 1)/n_study_m) %>%
  ungroup()

# Unique meta_ids
numbers <- df$meta_id %>% unique()

# Function to Process Subsets
process_subset <- function(data, condition, suffix) {
  condition_expr <- rlang::parse_expr(condition) # Parse the condition into an expression
  
  subset_data <- data %>%
    filter(!!condition_expr) %>% # Use the parsed expression
    select(bs, sebs, Ns, studyid, meta_id, onecoefm, ncoefm)
  
  source("maivebiasFE.R")
  
  results <- as.data.frame(t(MAIVEresults))
  colnames(results) <- results[1, ]
  results <- results[-1, ] %>%
    mutate(across(everything(), as.numeric))

  # Add row names as a new column in the data frame
  results_with_row_names <- results %>% 
    tibble::add_column(Row_Names = row.names(results), .before = 1)
  # Save the modified data frame to an Excel file
  write_xlsx(results_with_row_names, paste0("MAIVE", suffix, ".xlsx"))
   # write.csv(df$your_column_name, "your_column_name.csv", row.names = FALSE)
  results
}

# Assuming 'df' is your data frame and 'numbers' is a vector of 'meta_id' values
all <- df %>% 
  select(bs, sebs, Ns, studyid, meta_id, onecoef, ncoefm)
# Split 'all' into a list of data frames, each one corresponding to a unique 'meta_id'
list_of_meta <- split(all, all$meta_id)
# Optionally, if you want to name the list elements with 'Meta_' prefix
names(list_of_meta) <- paste0("Meta_", names(list_of_meta))

# Applying the Function to Different Subsets
object<-c("E","SE","F","Hausman","CV_of_Chi2", "Obs")
allMAIVE1 <- process_subset(df, "TRUE", "all")


object<-c("pE","pSE","pF","pHausman","pCV" , "pObs")
publishedMAIVE <- process_subset(df, "df$studyPublishD == 1", "p")

object<-c("npE","npSE","npF","npHausman","npCV" , "npObs")
neverPublishedMAIVE <- process_subset(df, "np == 1 & studyPublishD == 0", "np")

object<-c("wpE","wpSE","wpF","wpHausman","wpCV" ,"wpObs")
workingPapersMAIVE <- process_subset(df, "studyPublishD == 0", "wp")

object<-c("wpsE","wpsSE","wpsF","wpsHausman","wpsCV", "wpsObs")
workingPapersSansNpMAIVE <- process_subset(df, "studyPublishD == 0 & np == 0", "wps")

# Merging Results
merged_results <- list(allMAIVE, publishedMAIVE, neverPublishedMAIVE, workingPapersMAIVE, workingPapersSansNpMAIVE) %>%
  reduce(full_join, by = "metaID")

# Save Merged Results
write_xlsx(merged_results, "MAIVEresults_2N.xlsx")

# Cleanup
rm(list = setdiff(ls(), "merged_results"))
