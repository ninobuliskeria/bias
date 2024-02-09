#HISTOGRAM ####
library(dplyr)
library(weights)
library("readxl")
# rm(list = ls())
#setwd("H:/My Drive/BIAS/GitHub/Histogram-z")
# Import data ####
#dfreg  <- read_excel("H:/My Drive/BIAS/DATA/DataWorkReg.xlsx")  

title <- "z-statistics with mean coef and st.error"
unique_metaIDs <- unique(dfreg$metaID)
 large_metaIDs <- c(1,	18, 59,	65,	77,	108,	116,	132,	138,	142,
                    156,	178,	179,	181,	188,  191,	233,	247,	258,
                    291,	359,	367,	377,	378,	379,	385,	395,	415,
                    416,	421,	431,	435,	487,	511,	520,	528,	529,
                    547,	550,	571,	609,	610)
x <- dfreg %>%
 mutate(
   E = ifelse(E == 0, 0.0001, E),
   SE = ifelse(SE == 0, 0.0001, SE),
   x = abs(E / SE)
 ) 
x<- x[x$x != 1,]
# for (n in unique_metaIDs[298:412]) {
setwd("H:/My Drive/BIAS/GitHub/Histogram-z/large")
for (n in large_metaIDs) {
   #   for (n  in c(1, 2, 3)) {
  loop <- x %>%
    filter(studyPublishD == 1, SE < 10000, metaID != "609")#, metaID == n) 

# Step 1: Create the histogram and store bin information

 hist_data <- hist(loop[loop$x < 10 & loop$metaID !="609",]$x, breaks = 300, plot = FALSE)

  # filtered_df <- df[df$t_stat_sm < 10 & df$metaID != "609", ]
# weighted_data <- rep(filtered_df$t_stat_sm, filtered_df$weight_article_round)
# hist_data <- wtd.hist(df[df$t_stat_sm<10,]$t_stat_sm,weight=df[df$t_stat_sm<10,]$weight_article_round, breaks = 300, plot = FALSE)

# Step 2: Calculate the means for E and SE within each bin
bin_means_E <- numeric(length(hist_data$breaks)-1)
bin_means_SE <- numeric(length(hist_data$breaks)-1)

for (i in 1:(length(hist_data$breaks)-1)) {
  bin_indices <- which(loop$x >= hist_data$breaks[i] & loop$x < hist_data$breaks[i+1])
  bin_means_E[i] <- mean(loop$E[bin_indices])
  bin_means_SE[i] <- mean(loop$SE[bin_indices])
}

#Rescale
k=10
bin_means_E <- (bin_means_E)*k
bin_means_SE <- (bin_means_SE)*k

# First, make the barplot
barplot_heights <- hist_data$counts
barplot_names <- hist_data$mids
valmax <- max(c(!is.na(bin_means_E), !is.na(bin_means_SE), barplot_heights))
valmin <- min(c(!is.na(bin_means_E), !is.na(bin_means_SE),0))

# Open a PNG device
# png(paste0("2metaID_", n, ".png"), width=1600, height=1200, res=150)

bp <- barplot(barplot_heights, names.arg=barplot_names, ylim=c(valmin, valmax))#, xlab="z-test", ylab="Frequency")#, main="z-statistics and mean of coefficient and standard error estimates")
mtext(title, side = 3, line = 1, cex =.75)

# Find the index of the bar closest to 1.65
target_value1 <- 1.65
closest_index1 <- which.min(abs(barplot_names - target_value1))
abline(v = bp[closest_index1], col = "red", lwd = 2)

# Find the index of the bar closest to 1.95
target_value2 <- 1.95
closest_index2 <- which.min(abs(barplot_names - target_value2))
abline(v = bp[closest_index2], col = "red", lwd = 2)

# Find the index of the bar closest to 2.58
target_value3 <- 2.58
closest_index3 <- which.min(abs(barplot_names - target_value3))
abline(v = bp[closest_index3], col = "red", lwd = 2)

# Now add points for the E_means and SE_means
points(bp, bin_means_E, col='orange', pch=19)
points(bp, bin_means_SE, col='darkgreen', pch=19)

axis(4, at=axTicks(2), labels=axTicks(2)/k, las=3, cex =.75)

# Add a legend to differentiate the points
legend("topright", legend=c("mean coef", "mean st. errors"), pch=16, col=c("orange", "darkgreen"), cex =.75)
# Close the device to save the file
dev.off()
}
# Save the Figure 
# dev.off()


