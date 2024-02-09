# R code to create a publication-quality plot of estimates vs. standard errors
set.seed(123) # For reproducibility

# Generate standard errors that increase as the index increases
standard_errors <- seq(0.1, 1, length.out = 400)

# Generate estimates where the variance scales with the standard error
# Ensuring non-negative estimates
estimates <- abs(rnorm(n = 400, mean = 1, sd = standard_errors))

# Define plot margins to ensure labels and title fit well within the plot area
par(mar=c(5, 5, 4, 2) + 0.1)


# Plotting Estimates (E) against Standard Errors (SE)
plot(standard_errors, estimates, 
     # main = "Selection in Estimates",      # Title of the plot
     xlab = "Standard Errors (SE)",       # Label for the x-axis
     ylab = "Estimates (E)",              # Label for the y-axis
     pch = 21,                            # Point type (circles with borders)
     col = rgb(0, 0, 0, 0.5),             # Color of points (semi-transparent black)
     bg = rgb(0, 0, 0, 0.5),              # Background color of points
     ylim = c(0, max(1.96 * standard_errors)), # Setting y-axis limits
     cex.lab = 1.1,                       # Size of axis labels
     cex.main = 1.2,                      # Size of the main title
     bty = "l"                            # Type of box around the plot
)
# plot(estimates, standard_errors)

# # Adding a reference line
# model <- lm(estimates ~ standard_errors)
# abline(model, col="forestgreen", lwd=1)
# 
# #abline(a = 1, b = 0, col = rgb(0, 0, 0, 1), lwd = 1)
# 
# # Adding annotations (if needed)
# text(x = .1, y = 1.1, 
#      labels = "E = a + b SE", 
#      pos = 4, 
#      cex = 1.1, 
#      col = "forestgreen")



#Plot 2; Selection on t = 1.96
# Create the scatter plot with defined y-axis limits and enhanced formatting
plot(estimates, standard_errors,  
     # main="Selection in Estimates", # Title
     xlab="Standard Errors (SE)", # x-axis label
     ylab="Estimates (E)", # y-axis label
     pch=21, # Type of point (circles with borders)
     col=ifelse(estimates < 1.96 * standard_errors, "black", rgb(0, 0, 0, 0.5)), # Conditional color
     bg=ifelse(estimates < 1.96 * standard_errors, "white", rgb(0, 0, 0, 0.5)), # Background color of points
     xlim=c(0, max(1.96 * standard_errors)), # y-axis limits
     cex.lab=1.1, # Size of axis labels
     cex.main=1.2, # Size of the main title
     bty="l" # Type of box around the plot
)

# Add a diagonal line where estimates = 1.96 * SE starting from y=0
abline(a=0, b=1.96, col="red", lwd=2, lty=2)

# Adding annotations if needed
text(x=.8, y=1.9, labels="t=1.96", pos=4, cex=1.1, col="red")

# # Subset data for the regression line
# above_line <- estimates > 1.96 * standard_errors
# 
# # Fit a linear model only to the subset of data
# model_above_line <- lm(estimates[above_line] ~ standard_errors[above_line])
# mean <- mean(estimates[above_line])
# abline(a=mean, b=0, col="blue", lwd=1)
# 
# # Add the regression line to the plot for the subset
# abline(model_above_line, col="blue", lwd=1)
# # Adding annotations if needed
# text(x=.4, y=1.5, labels="E = a + b SE ", pos=3, cex=1, col="blue")


#plot 3, phacking 
# Your original plot code
plot(standard_errors, estimates, 
     # main="Selection in Estimates", # Title
     xlab="Standard Errors (SE)", # x-axis label
     ylab="Estimates (E)", # y-axis label
     pch=21, # Type of point (circles with borders)
     col=ifelse(estimates < 1.96 * standard_errors, "black", rgb(0, 0, 0, 0.5)), # Conditional color
     bg=ifelse(estimates < 1.96 * standard_errors, "white", rgb(0, 0, 0, 0.5)), # Background color of points
     ylim=c(0, max(1.96 * standard_errors)), # y-axis limits
     cex.lab=1.1, # Size of axis labels
     cex.main=1.2, # Size of the main title
     bty="l" # Type of box around the plot
)
abline(a=0, b=1.96, col="red", lwd=2, lty=2) # Diagonal line
text(x=.8, y=1.9, labels="t=1.96", pos=4, cex=1.1, col="red") # Annotation


manipulated_indices <- which(estimates < 1.96 * standard_errors)
manipulated_y_values <- 1.96 * standard_errors[manipulated_indices]
selected_indices <- c( 60, 72, 94, 110, 114, 
                       157, 167, 188, 196, 200, 
                       227, 238,
                       280, 290, 307, 350, 371, 389) # Replace with the indices of your choice

manipulated_y_values <- 1.96 * standard_errors[selected_indices]

points(standard_errors[selected_indices], manipulated_y_values, 
       pch=21, col="blue", bg="blue")





# Load ggplot2 package
library(ggplot2)

# Generate sample data
set.seed(567) # for reproducibility
x <- 1:100
y <- 2 * x + rnorm(100, mean = 0, sd = 50) # add some noise

data <- data.frame(x, y)

# Create scatter plot with regression line
ggplot(data, aes(x = x, y = y)) +
  geom_point() +  # Scatter plot
  geom_smooth(method = "lm", se = FALSE) +  # Regression line
  theme_minimal() +  # Minimal theme
  annotate("text", x = 82, y = 150, label = "y==a+hat(alpha)*x+u", parse = TRUE, size = 5, color = "red")  # Annotation

