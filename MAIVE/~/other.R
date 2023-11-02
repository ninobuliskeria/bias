
# Function to Process Subsets
# Function to Process Subsets
process_subset <- function(data, condition, suffix, x) {
  condition_expr <- rlang::parse_expr(condition) # Parse the condition into an expression
  
  subset_data <- data %>%
    filter(!!condition_expr) %>% # Use the parsed expression
    select(bs, sebs, Ns, studyid, meta_id) #, onecoefm, ncoefm)
  
  # source("maivebias.R")
  # setwd("H:/My Drive/BIAS/bias/MAIVE/Results")
  
  source("maivebiasFE.R")
  # setwd("H:/My Drive/BIAS/bias/MAIVE/ResultsFE")
  
  results <- as.data.frame(t(MAIVEresults))
  colnames(results) <- results[1, ]
  results <- results[-1, ] %>%
    mutate(across(everything(), as.numeric))
  
  # Add row names as a new column in the data frame
  results_with_row_names <- results %>% 
    tibble::add_column(Row_Names = row.names(results), .before = 1)
  
  # Save the modified data frame to an Excel file
  write_xlsx(results_with_row_names, paste0("MAIVE_", suffix, x, ".xlsx"))
  # write.csv(df$your_column_name, "your_column_name.csv", row.names = FALSE)
  
  results
}


# Applying the Function to Different Subsets
object<-c("E","SE","F","Hausman","CV_of_Chi2", "Obs")
allMAIVE <- process_subset(df, "TRUE", "all", "FE")

object<-c("pE","pSE","pF","pHausman","pCV" , "pObs")
publishedMAIVE <- process_subset(df, "studyPublishD == 1", "p", "FE")

object<-c("npE","npSE","npF","npHausman","npCV" , "npObs")
neverPublishedMAIVE <- process_subset(df, "np == 1 & studyPublishD == 0", "np", "FE")

object<-c("wpE","wpSE","wpF","wpHausman","wpCV" ,"wpObs")
workingPapersMAIVE <- process_subset(df, "studyPublishD == 0", "wp", "FE")

object<-c("wpsE","wpsSE","wpsF","wpsHausman","wpsCV", "wpsObs")
workingPapersSansNpMAIVE <- process_subset(df, "studyPublishD == 0 & np == 0", "wps", "FE")

# # Merging Results
# merged_results <- list(allMAIVE, publishedMAIVE, neverPublishedMAIVE, workingPapersMAIVE, workingPapersSansNpMAIVE) %>%
#   reduce(full_join, by = "metaID")
# 
# # Save Merged Results
# write_xlsx(merged_results, "MAIVEresults.xlsx")
# 
# # Cleanup
# rm(list = setdiff(ls(), "merged_results"))