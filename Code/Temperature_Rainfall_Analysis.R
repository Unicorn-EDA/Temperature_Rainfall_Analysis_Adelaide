# ------------------------------------------
# PART 1: RAINFALL DATA EXPLORATION
# ------------------------------------------

# Load required libraries for visualization and data manipulation
library(ggplot2)
library(dplyr)

# Read the raw monthly rainfall data set
Rainfall<- read.csv("Rainfall.csv")

# Convert the Year column to a factor so it can be treated as categorical data for box plot
Rainfall$Year <- as.factor(Rainfall$Year) 

# Generate a box plot to view rainfall distribution and identify anomalies across different years
ggplot(Rainfall, aes(x = Year, y = Rainfall)) +
  geom_boxplot(color = "black", fill = "deepskyblue") +
  labs(title = "Rainfall-Box Plot",
       x = "Year",
       y = "Rainfall (mm)") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=90,vjust=0.5,hjust=1))

# Compute comprehensive summary statistics for the rainfall data set
summary_stats <- Rainfall %>%
  summarise(Minimum = min(Rainfall,na.rm=TRUE),
            Q1 = quantile(Rainfall, p = 0.25,na.rm=TRUE),
            Median = median(Rainfall,na.rm=TRUE),
            Mean = mean(Rainfall,na.rm=TRUE),
            Q3 = quantile(Rainfall, p = 0.75,na.rm=TRUE),
            Maximum = max(Rainfall,na.rm=TRUE),
            IQR = IQR(Rainfall,na.rm=TRUE),
            Std = sd(Rainfall,na.rm=TRUE),
            Variance = var(Rainfall,na.rm=TRUE))
print(summary_stats)

# Plot a histogram to evaluate the distribution shape (skewness) of rainfall data
ggplot(Rainfall, aes(x = Rainfall)) +
  geom_histogram(color = "black", fill = "deepskyblue", bins = 30) +
  xlab("Rainfall (mm)") +
  ylab("Frequency(Counts of Months)") +
  ggtitle("Distribution of Rainfall in Adelaide") +
  theme_bw()

# Create a Normal Q-Q Plot to visually check if the rainfall data follows a normal distribution
ggplot(Rainfall, aes(sample = Rainfall)) +
  stat_qq(color = "black", alpha = 0.7) +         
  stat_qq_line(color = "red", linewidth = 1) +    
  labs(title = "Normal Q-Q Plot: Mean Rainfall",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles (millimeter)") +
  theme_bw() +                                    
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 11)
  )


# ------------------------------------------
# PART 2: TEMPERATURE DATA EXPLORATION
# ------------------------------------------

# Read the raw monthly temperature data set
Temperature<- read.csv("Temperature.csv")

# Convert the Year column to a factor so it can be treated as categorical data for box plot
Temperature$Year <- as.factor(Temperature$Year)

# Generate a box plot to view rainfall distribution and identify anomalies across different years
ggplot(Temperature, aes(x = Year, y = Temperature)) +
  geom_boxplot(color = "black", fill = "deepskyblue") +
  labs(title = "Temperature-Box Plot",
       x = "Year",
       y = "Temperature C") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=90,vjust=0.5,hjust=1))

# Compute comprehensive summary statistics for the temperature data set
summary_stats <- Temperature %>%
  summarise(Minimum = min(Temperature,na.rm=TRUE),
            Q1 = quantile(Temperature, p = 0.25,na.rm=TRUE),
            Median = median(Temperature,na.rm=TRUE),
            Mean = mean(Temperature,na.rm=TRUE),
            Q3 = quantile(Temperature, p = 0.75,na.rm=TRUE),
            Maximum = max(Temperature,na.rm=TRUE),
            IQR = IQR(Temperature,na.rm=TRUE),
            Std = sd(Temperature,na.rm=TRUE),
            Variance = var(Temperature,na.rm=TRUE))
print(summary_stats)

# Plot a histogram to check the distribution shape of temperature data (reveals seasonal bi-modal peaks)
ggplot(Temperature, aes(x = Temperature)) +
  geom_histogram(color = "black", fill = "deepskyblue", bins = 30) +
  xlab("Temperature(\u00B0C)") +
  ylab("Frequency(Counts of Months)") +
  ggtitle("Distribution of Temperature in Adelaide") +
  theme_bw()

# Create a Normal Q-Q Plot to visually test if temperature data meets normality requirements
ggplot(Temperature, aes(sample = Temperature)) +
  stat_qq(color = "black", alpha = 0.7) +         
  stat_qq_line(color = "red", linewidth = 1) +    
  labs(title = "Normal Q-Q Plot: Mean Temperature",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles (Celsius)") +
  theme_bw() +                                    
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 11)
  )


# ------------------------------------------
# PART 3: BIVARIATE & REGRESSION ANALYSIS
# ------------------------------------------

# Load tidyverse library to access advanced data combining tools
library(tidyverse)

# Reload raw datasets for fresh combining parameters
Rainfall<- read.csv("Rainfall.csv")
Temperature<- read.csv("Temperature.csv")

# Synchronize and join both datasets perfectly matching them by common 'Year' and 'Month'
Combined_data <- inner_join(Rainfall, Temperature, by = c("Year", "Month"))

# Generate a bivariate scatter plot to observe the relationship trend between Temperature and Rainfall
ggplot(Combined_data, aes(x = Temperature, y = Rainfall)) +
  geom_point(shape = 19, color = "black", alpha = 1, size = 1.5) + 
  geom_smooth(method = "lm", color = "red", se = FALSE, size = 1.2) +      
  labs(
    title = "Scatter Plot: Temperature vs Rainfall",
    subtitle = "Data synchronized by Year and Month",
    x = "Mean Temperature (°C)",
    y = "Total Rainfall (mm)"
  ) +
  theme_bw()

# Execute the non-parametric Spearman Rank Correlation test (handles the skewed rainfall data distribution)
cor_test<-cor.test(Combined_data$Temperature, Combined_data$Rainfall, method="spearman")
print(cor_test)

# Build the simple linear regression model to quantify the exact mathematical impact
model<-lm(Rainfall~Temperature, data=Combined_data)

# Print the model summary showing the intercept, slope coefficient, and R-squared value (0.32)
summary(model)


# ------------------------------------------
# PART 4: REGRESSION DIAGNOSTICS
# ------------------------------------------

# Configure the plotting area to display a 2x2 grid so all 4 diagnostic plots show at once
par(mfrow=c(2,2))

# Generate the 4 standard residual diagnostic plots (Residuals vs Fitted, Normal Q-Q, Scale-Location, Residuals vs Leverage)
# These check regression assumptions like constant variance (homoscedasticity) and normality of errors.
plot(model)

