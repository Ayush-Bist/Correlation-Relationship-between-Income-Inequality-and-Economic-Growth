# Load required libraries
library(tidyverse)
library(corrplot)
library(Hmisc)
library(ggplot2)
library(reshape2)
library(stats)
library(psych)

# Read the data with header names explicitly specified
data <- read.csv("C:/Users/ayush/OneDrive/Desktop/Inequality in Income.csv", 
                 header = TRUE, 
                 check.names = FALSE,
                 stringsAsFactors = FALSE)

# Print initial data structure
print("Initial data structure:")
str(data)

# Data Cleaning and Preprocessing
# Convert blank/empty strings to NA
data[data == ""] <- NA

# Select only the inequality columns (2010-2021)
inequality_cols <- data %>%
  select(matches("Inequality in income \\(\\d{4}\\)"))

# Convert columns to numeric and handle potential conversion issues
inequality_cols <- as.data.frame(lapply(inequality_cols, function(x) {
  as.numeric(gsub(",", "", as.character(x)))
}))

# Print data structure after cleaning
print("\nStructure after cleaning:")
str(inequality_cols)

# Basic Summary Statistics
if(ncol(inequality_cols) > 0) {
  summary_stats <- summary(inequality_cols)
  print("\nSummary Statistics:")
  print(summary_stats)
  
  # Calculate number of missing values per column
  missing_values <- colSums(is.na(inequality_cols))
  print("\nMissing Values per Column:")
  print(missing_values)
  
  # Correlation Analysis
  if(nrow(inequality_cols) > 1) {
    # Remove any columns with all NA values
    inequality_cols_clean <- inequality_cols[, colSums(!is.na(inequality_cols)) > 0]
    
    # Pearson Correlation
    pearson_cor <- cor(inequality_cols_clean, use = "pairwise.complete.obs", method = "pearson")
    print("\nPearson Correlation Matrix:")
    print(round(pearson_cor, 3))
    
    # Spearman Correlation
    spearman_cor <- cor(inequality_cols_clean, use = "pairwise.complete.obs", method = "spearman")
    print("\nSpearman Correlation Matrix:")
    print(round(spearman_cor, 3))
    
    # Correlation Heatmap
    png("correlation_heatmap.png", width = 800, height = 800)
    corrplot(pearson_cor, method = "color", type = "upper", 
             tl.col = "black", tl.srt = 45,
             title = "Pearson Correlation Heatmap")
    dev.off()
  }
}

# Reshape data for time series plot
inequality_long <- data %>%
  select(Country, Continent, `Human Development Groups`, 
         matches("Inequality in income \\(\\d{4}\\)")) %>%
  pivot_longer(
    cols = matches("Inequality in income \\(\\d{4}\\)"),
    names_to = "Year",
    values_to = "Inequality"
  ) %>%
  mutate(
    Year = as.numeric(str_extract(Year, "\\d{4}")),
    Inequality = as.numeric(gsub(",", "", as.character(Inequality)))
  ) %>%
  filter(!is.na(Inequality))  # Remove rows with NA values

# Calculate mean inequality by continent
mean_by_continent <- inequality_long %>%
  group_by(Continent, Year) %>%
  summarise(
    mean_inequality = mean(Inequality, na.rm = TRUE),
    n = n(),
    .groups = 'drop'
  ) %>%
  filter(!is.na(mean_inequality))

# Visualization 2: Time Series Plot by Development Groups
p1 <- ggplot(inequality_long, aes(x = Year, y = Inequality, color = `Human Development Groups`)) +
  geom_smooth(method = "loess", se = TRUE, na.rm = TRUE) +
  theme_minimal() +
  labs(title = "Inequality Trends by Human Development Groups",
       x = "Year",
       y = "Inequality Index") +
  theme(legend.position = "bottom")
print(p1)
ggsave("inequality_trends.png", plot = p1, width = 10, height = 6)

# Visualization 3: Boxplot by Continent
p2 <- ggplot(inequality_long, aes(x = Continent, y = Inequality, fill = Continent)) +
  geom_boxplot(na.rm = TRUE) +
  theme_minimal() +
  labs(title = "Income Inequality Distribution by Continent",
       x = "Continent",
       y = "Inequality Index") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p2)
ggsave("inequality_boxplot.png", plot = p2, width = 10, height = 6)

# Statistical Tests
# Shapiro-Wilk test for 2021 data
inequality_2021 <- inequality_long %>%
  filter(Year == 2021) %>%
  pull(Inequality)

if(length(na.omit(inequality_2021)) >= 3 && length(na.omit(inequality_2021)) <= 5000) {
  shapiro_test <- shapiro.test(na.omit(inequality_2021))
  print("\nShapiro-Wilk Normality Test for 2021 data:")
  print(shapiro_test)
}

# ANOVA test
if(nrow(inequality_long) > 0) {
  anova_result <- aov(Inequality ~ Continent, data = inequality_long)
  print("\nANOVA Test Results:")
  print(summary(anova_result))
}

# Descriptive statistics by Human Development Groups
desc_stats <- inequality_long %>%
  group_by(`Human Development Groups`) %>%
  summarise(
    N = n(),
    Mean = mean(Inequality, na.rm = TRUE),
    SD = sd(Inequality, na.rm = TRUE),
    Median = median(Inequality, na.rm = TRUE),
    IQR = IQR(Inequality, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  filter(!is.na(Mean))

print("\nDescriptive Statistics by Development Groups:")
print(desc_stats)

# Export results to CSV
write.csv(desc_stats, "descriptive_statistics.csv", row.names = FALSE)
if(exists("pearson_cor")) {
  write.csv(pearson_cor, "pearson_correlation.csv")
  write.csv(spearman_cor, "spearman_correlation.csv")
}

# Export correlation matrices to CSV
if (exists("pearson_cor") && exists("spearman_cor")) {
  # Convert correlation matrices to tidy format
  pearson_cor_tidy <- as.data.frame(as.table(pearson_cor))
  spearman_cor_tidy <- as.data.frame(as.table(spearman_cor))
  
  # Add columns to distinguish between Pearson and Spearman correlations
  pearson_cor_tidy$correlation_type <- "Pearson"
  spearman_cor_tidy$correlation_type <- "Spearman"
  
  # Combine the two data frames
  combined_correlations <- bind_rows(pearson_cor_tidy, spearman_cor_tidy)
  
  # Rename columns for clarity
  colnames(combined_correlations) <- c("Var1", "Var2", "Correlation_Coefficient", "Correlation_Type")
  
  # Write to CSV
  write.csv(combined_correlations, "correlation_coefficients.csv", row.names = FALSE)
}

# Descriptive Statistics by Continent
desc_stats_continent <- inequality_long %>%
  group_by(Continent) %>%
  summarise(
    N = n(),
    Mean = mean(Inequality, na.rm = TRUE),
    SD = sd(Inequality, na.rm = TRUE),
    Median = median(Inequality, na.rm = TRUE),
    IQR = IQR(Inequality, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  filter(!is.na(Mean))

# Save descriptive statistics by continent to CSV
write.csv(desc_stats_continent, "descriptive_statistics_by_continent.csv", row.names = FALSE)

# Time Series Plot by Continent (for each continent separately)
p3 <- ggplot(inequality_long, aes(x = Year, y = Inequality, color = Continent)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Time Series of Inequality by Continent",
       x = "Year",
       y = "Inequality Index") +
  theme(legend.position = "bottom")
print(p3)
ggsave("inequality_time_series_by_continent.png", plot = p3, width = 10, height = 6)

# Correlation Summary Table (positive/negative correlations)
positive_correlations <- sum(pearson_cor > 0, na.rm = TRUE)
negative_correlations <- sum(pearson_cor < 0, na.rm = TRUE)

correlation_summary <- data.frame(
  Correlation_Type = "Pearson",
  Positive_Correlations = positive_correlations,
  Negative_Correlations = negative_correlations
)

# For Spearman correlations
positive_correlations_spearman <- sum(spearman_cor > 0, na.rm = TRUE)
negative_correlations_spearman <- sum(spearman_cor < 0, na.rm = TRUE)

correlation_summary_spearman <- data.frame(
  Correlation_Type = "Spearman",
  Positive_Correlations = positive_correlations_spearman,
  Negative_Correlations = negative_correlations_spearman
)

# Combine summary for both Pearson and Spearman
combined_correlation_summary <- bind_rows(correlation_summary, correlation_summary_spearman)

# Save correlation summary to CSV
write.csv(combined_correlation_summary, "correlation_summary.csv", row.names = FALSE)

# Shapiro-Wilk Normality Test Results for 2021 data
shapiro_test_2021 <- shapiro.test(na.omit(inequality_2021))
shapiro_wilk_results <- data.frame(
  Test = "Shapiro-Wilk",
  W_Statistic = shapiro_test_2021$statistic,
  P_Value = shapiro_test_2021$p.value
)

# Save Shapiro-Wilk test results to CSV
write.csv(shapiro_wilk_results, "shapiro_wilk_test_results.csv", row.names = FALSE)

# ANOVA Test Results by Continent
anova_result_summary <- summary(anova_result)
anova_results_df <- data.frame(
  Source = rownames(anova_result_summary[[1]]),
  Df = anova_result_summary[[1]]$Df,
  Sum_Sq = anova_result_summary[[1]]$`Sum Sq`,
  Mean_Sq = anova_result_summary[[1]]$`Mean Sq`,
  F_Value = anova_result_summary[[1]]$`F value`,
  Pr_F = anova_result_summary[[1]]$`Pr(>F)`
)

# Save ANOVA test results to CSV
write.csv(anova_results_df, "anova_test_results.csv", row.names = FALSE)

# Export Correlation Matrices
if (exists("pearson_cor") && exists("spearman_cor")) {
  # Convert correlation matrices to tidy format
  pearson_cor_tidy <- as.data.frame(as.table(pearson_cor))
  spearman_cor_tidy <- as.data.frame(as.table(spearman_cor))
  
  # Add columns to distinguish between Pearson and Spearman correlations
  pearson_cor_tidy$correlation_type <- "Pearson"
  spearman_cor_tidy$correlation_type <- "Spearman"
  
  # Combine the two data frames
  combined_correlations <- bind_rows(pearson_cor_tidy, spearman_cor_tidy)
  
  # Rename columns for clarity
  colnames(combined_correlations) <- c("Var1", "Var2", "Correlation_Coefficient", "Correlation_Type")
  
  # Write to CSV
  write.csv(combined_correlations, "correlation_coefficients.csv", row.names = FALSE)
}

# Final CSV exports
write.csv(inequality_long, "reshaped_inequality_data.csv", row.names = FALSE)
