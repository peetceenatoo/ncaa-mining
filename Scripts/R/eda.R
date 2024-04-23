library(rgl)
library(scatterplot3d)
library(ggplot2)
library(e1071)
library(MASS)

#import dataset
data_sporco1 <- `cbb.(1)`
##data correction

data <- data_sporco1 [, 3:21]

risultato <- as.factor(data_sporco1[,22])

diff_ADJ<-data_sporco1 [,5]-data_sporco1 [,6]

data_prov<-cbind(data_sporco1,diff_ADJ)

#nuova variabile
win_ratio <- data[,2]*100/data[,1]
min(win_ratio)
max(win_ratio)

data_new <- cbind(data, win_ratio)

dati_2015 <- data_new[data_sporco1$YEAR == 2015, ]
dati_2016 <- data_new[data_sporco1$YEAR == 2016, ]
dati_2017 <- data_new[data_sporco1$YEAR == 2017, ]
dati_2018 <- data_new[data_sporco1$YEAR == 2018, ]
dati_2019 <- data_new[data_sporco1$YEAR == 2019, ]


# Ordine delle colonne desiderato
desired_order <- c(1, 2, 20, 5, 3, 4, 6, 7, 8, 9, 10, 11, 12, 13, 18, 19, 14, 15, 16, 17)

# Riordina il dataset
dati_2015_reordered <- dati_2015[, desired_order]
dati_2015<-dati_2015_reordered

# Load necessary libraries
library(ggplot2)

# Summary Statistics
summary_stats <- summary(dati_2015)

# Histograms
histograms <- lapply(names(dati_2015), function(col_name) {
  ggplot(dati_2015, aes_string(x = col_name)) +
    geom_histogram(fill = "skyblue", color = "black", bins = 30) +
    labs(title = paste("Histogram of", col_name))
})

# Box Plots
box_plots <- lapply(names(dati_2015), function(col_name) {
  ggplot(dati_2015, aes_string(y = col_name)) +
    geom_boxplot(fill = "skyblue", color = "black") +
    labs(title = paste("Boxplot of", col_name))
})

# Print summary statistics
print(summary_stats)

# Plot histograms
for (i in 1:length(histograms)) {
  print(histograms[[i]])
}

# Plot box plots
for (i in 1:length(box_plots)) {
  print(box_plots[[i]])
}



# Function to create histograms
create_histograms <- function(data, columns) {
  plots <- lapply(columns, function(col) {
    ggplot(data, aes_string(x = col)) +
      geom_histogram(fill = "skyblue", color = "black", bins = 30) +
      labs(title = paste("Histogram of", col))
  })
  return(plots)
}

# Function to create boxplots
create_boxplots <- function(data, columns) {
  plots <- lapply(columns, function(col) {
    ggplot(data, aes_string(y = col)) +
      geom_boxplot(fill = "skyblue", color = "black") +
      labs(title = paste("Boxplot of", col))
  })
  return(plots)
}

# Divide columns into groups of 4
col_groups <- split(names(dati_2015), rep(1:ceiling(ncol(dati_2015)/4), each = 4, length.out = ncol(dati_2015)))

# Create histograms for each group of 4 columns
hist_plots <- lapply(col_groups, function(group) {
  plots <- create_histograms(dati_2015, group)
  gridExtra::grid.arrange(grobs = plots, ncol = 2)
})

# Create boxplots for each group of 4 columns
box_plots <- lapply(col_groups, function(group) {
  plots <- create_boxplots(dati_2015, group)
  gridExtra::grid.arrange(grobs = plots, ncol = 2)
})





# Load necessary libraries
library(dplyr)
library(ggplot2)

# Calculate skewness for each column
skewness_values <- sapply(dati_2015, skewness)

# Create a data frame with column names and skewness values
skewness_df <- data.frame(column = names(dati_2015), skewness = skewness_values)

# Sort the data frame by skewness values
skewness_df <- skewness_df %>% arrange(desc(skewness))

# Extract the top 3 positive skewness values (asymmetric to the right)
positive_skew <- skewness_df[1:3, ]

# Extract the top 3 negative skewness values (asymmetric to the left)
negative_skew <- skewness_df[nrow(skewness_df): (nrow(skewness_df) - 2), ]

# Plot the distributions with the top 3 positive skewness values
positive_plots <- lapply(positive_skew$column, function(col_name) {
  ggplot(dati_2015, aes_string(x = col_name)) +
    geom_histogram(fill = "skyblue", color = "black", bins = 30) +
    labs(title = paste("Distribution of", col_name, "(Positive Skewness)"))
})

# Plot the distributions with the top 3 negative skewness values
negative_plots <- lapply(negative_skew$column, function(col_name) {
  ggplot(dati_2015, aes_string(x = col_name)) +
    geom_histogram(fill = "salmon", color = "black", bins = 30) +
    labs(title = paste("Distribution of", col_name, "(Negative Skewness)"))
})

# Display the plots
print(positive_plots[[1]])
print(positive_plots[[2]])
print(positive_plots[[3]])

print(negative_plots[[1]])
print(negative_plots[[2]])
print(negative_plots[[3]])


# Perform Shapiro-Wilk test for normality of win_ratio
shapiro_test <- shapiro.test(dati_2015$win_ratio)

# Print the test results
print(shapiro_test)

# Fit a normal distribution to the data
fit <- fitdistr(dati_2015$win_ratio, "normal")

# Print the estimated parameters of the normal distribution
print(fit)

# Plot histogram of the column win_ratio
hist_plot <- ggplot(dati_2015, aes(x = win_ratio)) +
  geom_histogram(aes(y = ..density..), fill = "skyblue", color = "black", bins = 30) +
  labs(title = "Histogram of win_ratio with Fitted Normal Distribution",
       x = "win_ratio", y = "Density")

# Add fitted normal distribution curve
hist_plot <- hist_plot + 
  stat_function(fun = dnorm, args = list(mean = fit$estimate[1], sd = fit$estimate[2]), 
                color = "red", linewidth = 1)

# Display the plot
print(hist_plot)


# Fit a normal distribution to the data
fit <- fitdistr(dati_2015$ADJOE, "normal")

# Print the estimated parameters of the normal distribution
print(fit)

# Plot histogram of the column adjoe
hist_plot <- ggplot(dati_2015, aes(x = ADJOE)) +
  geom_histogram(aes(y = ..density..), fill = "skyblue", color = "black", bins = 30) +
  labs(title = "Histogram of ADJOE with Fitted Normal Distribution",
       x = "ADJOE", y = "Density")

# Add fitted normal distribution curve
hist_plot <- hist_plot + 
  stat_function(fun = dnorm, args = list(mean = fit$estimate[1], sd = fit$estimate[2]), 
                color = "red", linewidth = 1)

# Display the plot
print(hist_plot)





# Fit a normal distribution to the data
fit <- fitdistr(dati_2015$ADJDE, "normal")

# Print the estimated parameters of the normal distribution
print(fit)

# Plot histogram of the column adjoe
hist_plot <- ggplot(dati_2015, aes(x = ADJDE)) +
  geom_histogram(aes(y = ..density..), fill = "skyblue", color = "black", bins = 30) +
  labs(title = "Histogram of ADJDE with Fitted Normal Distribution",
       x = "ADJDE", y = "Density")

# Add fitted normal distribution curve
hist_plot <- hist_plot + 
  stat_function(fun = dnorm, args = list(mean = fit$estimate[1], sd = fit$estimate[2]), 
                color = "red", linewidth = 1)

# Display the plot
print(hist_plot)






# Fit a normal distribution to the data
fit <- fitdistr(dati_2015$X2P_O, "normal")

# Print the estimated parameters of the normal distribution
print(fit)

# Plot histogram of the column adjoe
hist_plot <- ggplot(dati_2015, aes(x = X2P_O)) +
  geom_histogram(aes(y = ..density..), fill = "skyblue", color = "black", bins = 30) +
  labs(title = "Histogram of X2P_O with Fitted Normal Distribution",
       x = "X2P_0", y = "Density")

# Add fitted normal distribution curve
hist_plot <- hist_plot + 
  stat_function(fun = dnorm, args = list(mean = fit$estimate[1], sd = fit$estimate[2]), 
                color = "red", linewidth = 1)

# Display the plot
print(hist_plot)



# Fit a normal distribution to the data
fit <- fitdistr(dati_2015$X3P_O, "normal")

# Print the estimated parameters of the normal distribution
print(fit)

# Plot histogram of the column adjoe
hist_plot <- ggplot(dati_2015, aes(x = X3P_O)) +
  geom_histogram(aes(y = ..density..), fill = "skyblue", color = "black", bins = 30) +
  labs(title = "Histogram of X3P_O with Fitted Normal Distribution",
       x = "X3P_0", y = "Density")

# Add fitted normal distribution curve
hist_plot <- hist_plot + 
  stat_function(fun = dnorm, args = list(mean = fit$estimate[1], sd = fit$estimate[2]), 
                color = "red", linewidth = 1)

# Display the plot
print(hist_plot)



# Fit a normal distribution to the data
fit <- fitdistr(dati_2015$X3P_D, "normal")

# Print the estimated parameters of the normal distribution
print(fit)

# Plot histogram of the column adjoe
hist_plot <- ggplot(dati_2015, aes(x = X3P_D)) +
  geom_histogram(aes(y = ..density..), fill = "skyblue", color = "black", bins = 30) +
  labs(title = "Histogram of X3P_D with Fitted Normal Distribution",
       x = "X3P_D", y = "Density")

# Add fitted normal distribution curve
hist_plot <- hist_plot + 
  stat_function(fun = dnorm, args = list(mean = fit$estimate[1], sd = fit$estimate[2]), 
                color = "red", linewidth = 1)

# Display the plot
print(hist_plot)




# Fit a normal distribution to the data
fit <- fitdistr(dati_2015$X2P_D, "normal")

# Print the estimated parameters of the normal distribution
print(fit)

# Plot histogram of the column adjoe
hist_plot <- ggplot(dati_2015, aes(x = X2P_D)) +
  geom_histogram(aes(y = ..density..), fill = "skyblue", color = "black", bins = 30) +
  labs(title = "Histogram of X2P_D with Fitted Normal Distribution",
       x = "X2P_D", y = "Density")

# Add fitted normal distribution curve
hist_plot <- hist_plot + 
  stat_function(fun = dnorm, args = list(mean = fit$estimate[1], sd = fit$estimate[2]), 
                color = "red", linewidth = 1)

# Display the plot
print(hist_plot)
