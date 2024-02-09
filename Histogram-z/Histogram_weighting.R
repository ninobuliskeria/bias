 
library(dplyr)
library(ggplot2)
library(weights)
library("readxl")
# rm(list = ls())
#setwd("H:/My Drive/BIAS/GitHub/Histogram-z")
# Data ####
## Import & edit ####
#dfreg  <- read_excel("H:/My Drive/BIAS/DATA/DataWorkReg.xlsx")  

df <- dfreg %>% 
  filter(E != SE, studyPublishD != 0 , SE < 10000 , metaID !="609" )%>%
  mutate(E = ifelse(E == 0, 0.0001, E),
         SE = ifelse(SE == 0, 0.0001, SE),
         coeff_num = E, 
         stdev_num = SE, 
         n = sample)

## Demean & weights #### 
df <- df %>%
  mutate(
    coeff_sm = coeff_num + (runif(n()) - 0.5) * 10^(-nchar(as.character(coeff_num))),
    stdev_sm = stdev_num + (runif(n()) - 0.5) * 10^(-nchar(as.character(stdev_num))),
    t_stat_raw = abs(coeff_num / stdev_num),
    t_stat_sm = abs(coeff_sm / stdev_sm)
  ) %>%
  filter(!(is.na(t_stat_raw) & !is.na(t_stat_sm))) %>%
  filter(t_stat_raw != 0, t_stat_sm != 0) %>%
  filter(t_stat_raw != 1)

# weighting 
df$weight_article <- 1 / df$n_E_s
df$weight_article_round=round(df$weight_article*10^8)

# Raw ####
# Step 1: Create the histogram and store bin information
hist_data <- hist(df[df$t_stat_raw<10,]$t_stat_raw, breaks = 300, plot = FALSE)
 
# Step 2: Calculate the means for E and SE within each bin
bin_means_E <- numeric(length(hist_data$breaks)-1)
bin_means_SE <- numeric(length(hist_data$breaks)-1)

for (i in 1:(length(hist_data$breaks)-1)) {
  bin_indices <- which(df$t_stat_raw >= hist_data$breaks[i] & df$t_stat_raw < hist_data$breaks[i+1])
  bin_means_E[i] <- mean(df$E[bin_indices])
  bin_means_SE[i] <- mean(df$SE[bin_indices])
}

#Rescale
k=500
means_x <- seq(from = 0, to = 10, length.out = 200)
means_data <- as.data.frame(means_x)
means_data$means_E <- (bin_means_E)/k
means_data$means_SE <- (bin_means_SE)/k


p0<-ggplot(df[df$t_stat_raw < 10,], aes(x = t_stat_raw)) +
  geom_histogram(aes(y = ..density..), bins = 200, color = "black", fill = "gray50") +
  geom_density(bw = 0.2, color = "black") +
  geom_point(data = means_data, aes(x = means_x, y = means_E, color = "Coefficients") ) +
  geom_point(data = means_data, aes(x = means_x, y = means_SE, color = "Standard Errors") ) +
  geom_vline(xintercept = 1.65, color = "red", linetype = "dashed", linewidth = 0.2) +
  geom_vline(xintercept = 1.96, color = "red", linetype = "dashed", linewidth = 0.2) +
  geom_vline(xintercept = 2.58, color = "red", linetype = "dashed", linewidth = 0.2) +
  geom_vline(xintercept = 3.29, color = "red", linetype = "dashed", linewidth = 0.2) +
  scale_x_continuous(breaks = c(0, 1.65, 1.95, 2.58, 3.29, 10), labels = c("0", "1.65", "1.95", "2.58", "3.29", "10")) +
  scale_y_continuous(breaks = seq(0, 0.4, by = 0.1), sec.axis = sec_axis(~ . * k, name = "")) +
  labs(x = "z-statistic", y = "Density", color = "") +
  scale_color_manual(values = c("Coefficients" = "darkgreen", "Standard Errors" = "orange")) +
  theme_minimal() +
  theme(legend.position = c(.9, .8),
        legend.box.background = element_blank(), #element_rect(color = "black", fill = "white"),
        legend.box.margin = margin(-15,3, 3, 3, unit = "pt"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
  )

ggsave("all.png", plot = p0, width = 10, height = 6)

# Demeaned ####
# Step 1: Create the histogram and store bin information
hist_data <- hist(df[df$t_stat_sm<10,]$t_stat_sm, breaks = 300, plot = FALSE)
 
# Step 2: Calculate the means for E and SE within each bin
bin_means_E <- numeric(length(hist_data$breaks)-1)
bin_means_SE <- numeric(length(hist_data$breaks)-1)

for (i in 1:(length(hist_data$breaks)-1)) {
  bin_indices <- which(df$t_stat_sm >= hist_data$breaks[i] & df$t_stat_sm < hist_data$breaks[i+1])
  bin_means_E[i] <- mean(df$E[bin_indices])
  bin_means_SE[i] <- mean(df$SE[bin_indices])
}

#Rescale
k=500
means_x <- seq(from = 0, to = 10, length.out = 200)
means_data <- as.data.frame(means_x)
means_data$means_E <- (bin_means_E)/k
means_data$means_SE <- (bin_means_SE)/k

p1<-ggplot(df[df$t_stat_sm < 10,], aes(x = t_stat_sm)) +
  geom_histogram(aes(y = ..density..), bins = 200, color = "black", fill = "gray50") +
  geom_density(bw = 0.2, color = "black") +
  geom_point(data = means_data, aes(x = means_x, y = means_E, color = "Coefficients")) +
  geom_point(data = means_data, aes(x = means_x, y = means_SE, color = "Standard Errors")) +
  geom_vline(xintercept = 1.65, color = "red", linetype = "dashed", linewidth = 0.2) +
  geom_vline(xintercept = 1.96, color = "red", linetype = "dashed", linewidth = 0.2) +
  geom_vline(xintercept = 2.58, color = "red", linetype = "dashed", linewidth = 0.2) +
  geom_vline(xintercept = 3.29, color = "red", linetype = "dashed", linewidth = 0.2) +
  scale_x_continuous(breaks = c(0, 1.65, 1.95, 2.58, 3.29, 10), labels = c("0", "1.65", "1.95", "2.58", "3.29", "10")) +
  scale_y_continuous(breaks = seq(0, 0.4, by = 0.1), sec.axis = sec_axis(~ . * k, name = "")) +
  labs(x = "z-statistic", y = "Density", color = "") +
  scale_color_manual(values = c("Coefficients" = "darkgreen", "Standard Errors" = "orange")) +
  theme_minimal() +
  theme(legend.position = c(.9, .8),
        legend.box.background = element_blank(), #element_rect(color = "black", fill = "white"),
        legend.box.margin = margin(-15,3, 3, 3, unit = "pt"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
  )

ggsave("allDE.png", plot = p1, width = 10, height = 6)

# Demeaned and weighted #### 
# Step 1: Create the histogram and store bin information
hist_data <- wtd.hist(df[df$t_stat_sm<10,]$t_stat_sm,weight=df[df$t_stat_sm<10,]$weight_article_round, breaks = 300, plot = FALSE)

# Step 2: Calculate the means for E and SE within each bin
bin_means_E <- numeric(length(hist_data$breaks)-1)
bin_means_SE <- numeric(length(hist_data$breaks)-1)

for (i in 1:(length(hist_data$breaks)-1)) {
  bin_indices <- which(df$t_stat_sm >= hist_data$breaks[i] & df$t_stat_sm < hist_data$breaks[i+1])
  bin_means_E[i] <- mean(df$E[bin_indices])
  bin_means_SE[i] <- mean(df$SE[bin_indices])
}

#Rescale\\
k=500
means_x <- seq(from = 0, to = 10, length.out = 200)
means_data <- as.data.frame(means_x)
means_data$means_E <- (bin_means_E)/k
means_data$means_SE <- (bin_means_SE)/k

# Plot
p2<-ggplot(df[df$t_stat_sm < 10,], aes(x = t_stat_sm, weight = weight_article_round)) +
      geom_histogram(aes(y = ..density..), bins = 200, color = "black", fill = "gray50") +
      geom_density(bw = 0.2, color = "black") +
      geom_point(data = means_data, aes(x = means_x, y = means_E, color = "Coefficients"), inherit.aes = FALSE) +
      geom_point(data = means_data, aes(x = means_x, y = means_SE, color = "Standard Errors"), inherit.aes = FALSE) +
      geom_vline(xintercept = 1.65, color = "red", linetype = "dashed", linewidth = 0.2) +
      geom_vline(xintercept = 1.96, color = "red", linetype = "dashed", linewidth = 0.2) +
      geom_vline(xintercept = 2.58, color = "red", linetype = "dashed", linewidth = 0.2) +
      geom_vline(xintercept = 3.29, color = "red", linetype = "dashed", linewidth = 0.2) +
      scale_x_continuous(breaks = c(0, 1.65, 1.95, 2.58, 3.29, 10), labels = c("0", "1.65", "1.95", "2.58", "3.29", "10")) +
      scale_y_continuous(breaks = seq(0, 0.4, by = 0.1), sec.axis = sec_axis(~ . * k, name = "")) +
      labs(x = "z-statistic", y = "Density", color = "") +
      scale_color_manual(values = c("Coefficients" = "darkgreen", "Standard Errors" = "orange")) +
      theme_minimal() +
      theme(legend.position = c(.9, .8),
            legend.box.background = element_blank(), #element_rect(color = "black", fill = "white"),
            legend.box.margin = margin(-15,3, 3, 3, unit = "pt"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()
            )

ggsave("allDEw.png", plot = p2, width = 10, height = 6)


 






 