# Load the installed packages
library(ggplot2)   # For visualization
library(dplyr)     # For data manipulation
library(tidyr)     # For data tidying
library(car)       # For ANOVA and advanced regression
library(readr)     # For reading CSV and other files
library(psych)     # For statistical analysis

# Load necessary libraries
library(ggplot2)   # For visualization
library(dplyr)     # For data manipulation
library(psych)     # For descriptive statistics
# Set seed for reproducibility
set.seed(123)
# Simulate data
n <- 100
social_class <- factor(
  sample(c("Upper Middle", "Lower Middle", "Upper Working", "Lower Working"), 
         n, replace = TRUE, 
         prob = c(0.25, 0.25, 0.25, 0.25))
)
books_read <- c(
  rpois(25, lambda = 20),  # Upper Middle class
  rpois(25, lambda = 15),  # Lower Middle class
  rpois(25, lambda = 8),   # Upper Working class
  rpois(25, lambda = 5)    # Lower Working class
)
# Create the data frame
data <- data.frame(SocialClass = social_class, BooksRead = books_read)
# View summary of the dataset
summary(data)

# Boxplot: Books read by social class
ggplot(data, aes(x = SocialClass, y = BooksRead, fill = SocialClass)) +
  geom_boxplot() +
  labs(title = "Distribution of Books Read by Social Class", 
       x = "Social Class", 
       y = "Number of Books Read") +
  theme_minimal() +
  theme(legend.position = "none")

# Encode social class as numeric
data$SocialClassNumeric <- as.numeric(data$SocialClass)

# Compute Pearson's correlation
correlation <- cor.test(data$SocialClassNumeric, data$BooksRead, method = "pearson")
correlation

# ANOVA to assess differences in books read across social classes
anova_result <- aov(BooksRead ~ SocialClass, data = data)
summary(anova_result)

# Scatterplot with regression line
ggplot(data, aes(x = SocialClassNumeric, y = BooksRead)) +
  geom_jitter(width = 0.2, alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Correlation Between Social Class and Books Read", 
       x = "Social Class (Numeric Encoding)", 
       y = "Number of Books Read") +
  theme_minimal()


# Load necessary library
library(psych)

# Define correlation coefficient and sample sizes
r_value <- 0.55
sample_sizes <- c(10, 30, 50, 100, 500)

# Function to calculate p-value for Pearson's correlation
correlation_p <- function(r, n) {
  t_value <- r * sqrt((n - 2) / (1 - r^2)) # Calculate t-statistic
  p_value <- 2 * pt(-abs(t_value), df = n - 2) # Two-tailed p-value
  return(p_value)
}

# Calculate p-values for different sample sizes
p_values <- sapply(sample_sizes, correlation_p, r = r_value)

# Create a data frame for results
results <- data.frame(SampleSize = sample_sizes, Correlation = r_value, P_Value = p_values)
print(results)


# Simulate data for two scenarios
set.seed(123)

# Scenario 1: r = 0.55, small sample size (n = 15)
n1 <- 15
x1 <- rnorm(n1, mean = 50, sd = 10) # Independent variable
y1 <- 0.55 * scale(x1) + rnorm(n1, sd = 1) # Dependent variable

# Scenario 2: r = 0.46, large sample size (n = 100)
n2 <- 100
x2 <- rnorm(n2, mean = 50, sd = 10)
y2 <- 0.46 * scale(x2) + rnorm(n2, sd = 1)

# Combine data into a data frame
data1 <- data.frame(x = x1, y = y1, Scenario = "Scenario 1 (r = 0.55, n = 15)")
data2 <- data.frame(x = x2, y = y2, Scenario = "Scenario 2 (r = 0.46, n = 100)")
combined_data <- rbind(data1, data2)



# Correlation for Scenario 1
correlation1 <- cor.test(data1$x, data1$y, method = "pearson")
print(correlation1)

# Correlation for Scenario 2
correlation2 <- cor.test(data2$x, data2$y, method = "pearson")
print(correlation2)

# Load ggplot2 for visualization
library(ggplot2)

# Scatterplots for both scenarios
ggplot(combined_data, aes(x = x, y = y, color = Scenario)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Correlation and Sample Size Effect",
       x = "Income",
       y = "Interest in Work") +
  theme_minimal() +
  theme(legend.position = "top")

