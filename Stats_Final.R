# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(psych)
library(stringr)


# Load the dataset
data <- read.csv("C:/Users/Akash/Desktop/Stats Project/Inequality in Income.csv")

# Data Cleaning ----------------------------------------------------------

# Step 1: Remove rows where HDI.Rank..2021. is missing
data <- data %>% 
  filter(!is.na(HDI.Rank..2021.))  # Keep rows with non-missing HDI Rank

# Step 2: Remove rows where any "Inequality" column has missing values
data <- data %>%
  drop_na(starts_with("Inequality"))


# Descriptive Statistics -------------------------------------------------

# Summary statistics for HDI Rank and Inequality columns
summary_stats <- data %>%
  select(-Country) %>%
  describe()

# Display the summary statistics
print(summary_stats)

# PLOTS --------------------------------------------
# 1. Mean HDI Rank by Continent (Barplot)
avg_hdi_rank <- tapply(data$HDI.Rank..2021., data$Continent, mean, na.rm = TRUE)
barplot(avg_hdi_rank, main = "Average HDI Rank by Continent", xlab = "Continent", ylab = "Average HDI Rank", col = rainbow(length(avg_hdi_rank)))

# 2. Average Inequality in Income from 2010 to 2021 by Continent (Line Plot)
inequality_columns <- grep("Inequality.in.income..20(1[0-9]|20)", names(data), value = TRUE)

# Step 1: Aggregate only over available year columns
avg_inequality_income <- aggregate(. ~ Continent, data = data[c("Continent", inequality_columns)], mean, na.rm = TRUE)

# Step 2: Extract years from column names and prepare for plotting
years <- as.numeric(sub("Inequality.in.income..", "", inequality_columns))

# Plotting average inequality in income by continent across available years
plot(years, avg_inequality_income[1, -1], type = "l", col = 1, lty = 1, ylim = range(avg_inequality_income[-1]), xlab = "Year", ylab = "Average Inequality in Income", main = "Average Inequality in Income (2010-2021) by Continent")
for (i in 1:nrow(avg_inequality_income)) {
  lines(years, avg_inequality_income[i, -1], col = i)
}

# Add legend
legend("topright", legend = avg_inequality_income$Continent, col = 1:nrow(avg_inequality_income), lty = 1)



# 3. Average Inequality in Income in 2010 - 2021 by Human Development Group (Bar Plot)
# Define the range of years to plot
years <- 2010:2021

# Set up the plotting window with a suitable grid layout
par(mfrow = c(4, 3), mar = c(4, 4, 2, 1))  # 4 rows, 3 columns, and adjust margins

# Loop over each year to create individual bar plots
for (year in years) {
  # Create the column name dynamically for each year
  year_column <- paste0("Inequality.in.income..", year, ".")
  
  # Calculate the average inequality for the year by Human Development Group
  avg_inequality <- tapply(data[[year_column]], data$Human.Development.Groups, mean, na.rm = TRUE)
  
  # Plot the bar chart for the current year
  barplot(
    avg_inequality,
    main = paste("Average Inequality in", year),
    xlab = "Human Development Group",
    ylab = "Average Inequality",
    col = rainbow(length(avg_inequality)),
    las = 2
  )
}

# Reset plotting layout
par(mfrow = c(1, 1))


# 4. Inequality in Income (2010-2021) for Top 5 and Bottom 5 Countries by HDI Rank (Line Plot)
data_sorted <- data[order(data$HDI.Rank..2021.), ]
top_5 <- data_sorted[1:5, ]
bottom_5 <- data_sorted[(nrow(data_sorted) - 4):nrow(data_sorted), ]

# Extract available years for inequality data
inequality_columns <- grep("Inequality.in.income..20(1[5-9]|20)", names(data), value = TRUE)
years <- as.numeric(sub("Inequality.in.income..", "", inequality_columns))

# Plot for Top 5 Countries
matplot(years, t(top_5[inequality_columns]), type = "l", lty = 1, col = 1:5,
        xlab = "Year", ylab = "Inequality", main = "Inequality in Income (Top 5 Countries)")
legend("topright", legend = top_5$Country, col = 1:5, lty = 1)

# Plot for Bottom 5 Countries
matplot(years, t(bottom_5[inequality_columns]), type = "l", lty = 1, col = 1:5,
        xlab = "Year", ylab = "Inequality", main = "Inequality in Income (Bottom 5 Countries)")
legend("topright", legend = bottom_5$Country, col = 1:5, lty = 1)

# 5. Average Inequality in Income by Human Development Group (Line Plot)
avg_inequality_by_group <- aggregate(. ~ Human.Development.Groups, 
                                     data = data[c("Human.Development.Groups", grep("Inequality.in.income", names(data), value = TRUE))], 
                                     mean, na.rm = TRUE)

# Define the years based on the inequality columns (2010 to 2021)
years <- as.character(2010:2021)

# Initialize the plot with the first Human Development Group's data
plot(years, avg_inequality_by_group[1, 2:13], type = "l", col = 1, lty = 1, 
     ylim = range(avg_inequality_by_group[, 2:13], na.rm = TRUE), 
     xlab = "Year", ylab = "Average Inequality", 
     main = "Average Inequality by Human Development Group")

for (i in 2:nrow(avg_inequality_by_group)) {
  lines(years, avg_inequality_by_group[i, 2:13], col = i)
}

legend("topright", legend = avg_inequality_by_group$Human.Development.Groups, 
       col = 1:nrow(avg_inequality_by_group), lty = 1)

# 6. Boxplot of Inequality in Income by Year
inequality_data <- data[, grep("Inequality.in.income", names(data))]
colnames(inequality_data) <- gsub("Inequality.in.income..", "", colnames(inequality_data))
boxplot(inequality_data, main = "Boxplot of Inequality in Income by Year", xlab = "Year", ylab = "Inequality in Income", col = "lightblue", las = 2)

# 7. Scatter Plot of HDI Rank vs. Inequality for Each Year (with Linear Trend)
inequality_years <- grep("Inequality.in.income", names(data), value = TRUE)

# Set up the plotting window with a suitable grid layout
par(mfrow = c(4, 3))

# Loop over each inequality column to create individual scatter plots
for (year in inequality_years) {
  # Clean the year label from the column name
  year_clean <- gsub("Inequality.in.income..", "", year)
  
  # Create the scatter plot for HDI Rank (2021) vs Inequality for the current year
  plot(
    data$HDI.Rank..2021., 
    data[[year]], 
    main = paste("HDI Rank (2021) vs Inequality (", year_clean, ")", sep = ""),
    xlab = "HDI Rank (2021)", 
    ylab = "Inequality in Income"
  )
  # Add a regression line
  abline(lm(data[[year]] ~ data$HDI.Rank..2021., data = data), col = "blue")
}

# Reset plotting layout
par(mfrow = c(1, 1))


#CORRELATION COMPUTING ------------------------------------------

#CORRELATION BETWEEN HDI RANK AND INEQUALITY (EACH YEAR) --------

# Calculate correlations between HDI Rank (2021) and each year's Inequality using Pearson and Spearman methods

# Select the relevant columns: 'HDI Rank (2021)' and Inequality columns
correlation_data <- data %>%
  select(HDI.Rank..2021., starts_with("Inequality.in.income")) %>%
  gather(key = "Year", value = "Inequality", -HDI.Rank..2021.)  # Reshape data

# Convert Inequality column to numeric
correlation_data$Inequality <- as.numeric(gsub(",", "", as.character(correlation_data$Inequality)))

# Pearson Correlation
pearson_cor_year_hdi <- correlation_data %>%
  group_by(Year) %>%
  summarise(pearson_corr = cor(HDI.Rank..2021., Inequality, method = "pearson", use = "complete.obs"))

# Spearman Correlation
spearman_cor_year_hdi <- correlation_data %>%
  group_by(Year) %>%
  summarise(spearman_corr = cor(HDI.Rank..2021., Inequality, method = "spearman", use = "complete.obs"))

# Print the correlation results
print("Pearson Correlation between HDI Rank and Inequality by Year:")
print(pearson_cor_year_hdi)

print("Spearman Correlation between HDI Rank and Inequality by Year:")
print(spearman_cor_year_hdi)

# Combine the Pearson and Spearman correlation results into one data frame
correlations_combined <- pearson_cor_year_hdi %>%
  left_join(spearman_cor_year_hdi, by = "Year") %>%
  gather(key = "Correlation_Type", value = "Correlation_Coefficient", pearson_corr, spearman_corr)

# Rename columns for clarity
colnames(correlations_combined) <- c("Year", "Correlation_Type", "Correlation_Coefficient")

# Plot correlations
ggplot(correlations_combined, aes(x = Year, y = Correlation_Coefficient, color = Correlation_Type, group = Correlation_Type)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(title = "Pearson and Spearman Correlations between HDI Rank and Inequality by Year",
       x = "Year", y = "Correlation Coefficient") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = c("blue", "red"))  # Blue for Pearson, Red for Spearman

# Export Pearson and Spearman correlation results to CSV
write.csv(pearson_cor_year_hdi, "pearson_correlation.csv", row.names = FALSE)
write.csv(spearman_cor_year_hdi, "spearman_correlation.csv", row.names = FALSE)

# Print the CSV export success message
print("Pearson and Spearman correlation coefficients have been saved to CSV files.")

#TIME SERIES ANALYSIS -----------------------

data <- read.csv("C:/Users/Akash/Desktop/Stats Project/Inequality in Income.csv", 
                 header = TRUE, 
                 check.names = FALSE,
                 stringsAsFactors = FALSE)


# Select only the inequality columns (2010-2021)
inequality_cols <- data %>%
  select(matches("Inequality in income \\(\\d{4}\\)"))

# Convert columns to numeric and handle potential conversion issues
inequality_cols <- as.data.frame(lapply(inequality_cols, function(x) {
  as.numeric(gsub(",", "", as.character(x)))
}))

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

# Final CSV exports
write.csv(inequality_long, "reshaped_inequality_data.csv", row.names = FALSE)

