install.packages(c("tidyverse", "corrplot", "Hmisc", "ggplot2", "reshape2", "psych", "ggcorrplot"))

# Load necessary libraries
library(tidyverse)
library(corrplot)
library(Hmisc)
library(ggplot2)
library(reshape2)
library(stats)
library(psych)
library(dplyr)
library(ggcorrplot)
library(tidyr)

# Read the data with header names explicitly specified
data <- read.csv("C:/Users/ayush/OneDrive/Desktop/Inequality in Income.csv", 
                 header = TRUE, 
                 check.names = FALSE,
                 stringsAsFactors = FALSE)

# Data Cleaning ----------------------------------------------------------

# Rename problematic column names for easier referencing in R 
colnames(data) <- make.names(colnames(data))
# Check new column names 
colnames(data)

# Proceed with the summary and missing values check
summary(data)

#Print the number of rows with missing values in each column 
colSums(is.na(data))

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

ggplot(data, aes(x = Continent, y = HDI.Rank..2021., fill = Continent)) +
  geom_bar(stat = "summary", fun = "mean") +
  labs(title = "Average HDI Rank by Continent", x = "Continent", y = "Average HDI Rank") +
  theme_minimal()

#Line Plot 
#Calculate the average inequality from 2015 to 2021 for each continent
avg_inequality_income_continent <- data %>%
  group_by(Continent) %>%
  summarise(
    `2015` = mean(Inequality.in.income..2015., na.rm = TRUE),
    `2016` = mean(Inequality.in.income..2016., na.rm = TRUE),
    `2017` = mean(Inequality.in.income..2017., na.rm = TRUE),
    `2018` = mean(Inequality.in.income..2018., na.rm = TRUE),
    `2019` = mean(Inequality.in.income..2019., na.rm = TRUE),
    `2020` = mean(Inequality.in.income..2020., na.rm = TRUE),
    `2021` = mean(Inequality.in.income..2021., na.rm = TRUE)
  ) %>%
  pivot_longer(cols = -Continent, names_to = "Year", values_to = "Average_Inequality")

# Convert Year column to numeric for proper plotting
avg_inequality_income_continent$Year <- as.numeric(avg_inequality_income_continent$Year)

# Line plot of average inequality in income from 2015 to 2021 by Continent
ggplot(avg_inequality_income_continent, aes(x = Year, y = Average_Inequality, color = Continent)) +
  geom_line(size = 1) +
  labs(title = "Average Inequality in Income from 2015 to 2021 by Continent",
       x = "Year", y = "Average Inequality in Income") +
  theme_minimal() +
  theme(legend.position = "right")

#Plot 3
# First, calculate the average inequality in income for each Human Development Group in 2021
avg_inequality_income_dev_groups <- data %>%
  group_by(Human.Development.Groups) %>%
  summarise(Average_Inequality_2021 = mean(Inequality.in.income..2021., na.rm = TRUE))

# Plotting the bar plot
ggplot(avg_inequality_income_dev_groups, aes(x = Human.Development.Groups, y = Average_Inequality_2021, fill = Human.Development.Groups)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Inequality in Income in 2021 by Human Development Group",
       x = "Human Development Group",
       y = "Average Inequality in Income (2021)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Plot 4
# Scatter plot of HDI Rank vs Inequality in Income for the year 2021
ggplot(data, aes(x = HDI.Rank..2021., y = Inequality.in.income..2021.)) +
  geom_point(color = "blue", alpha = 0.7) +
  labs(title = "HDI Rank vs Inequality in Income (2021)",
       x = "HDI Rank (2021)",
       y = "Inequality in Income (2021)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#Plot 5
# Step 1: Drop rows with missing HDI Rank (2021)
data_dropna <- data %>% filter(!is.na(HDI.Rank..2021.))

# Step 2: Get the top 5 and bottom 5 countries by HDI Rank (2021)
top_5_countries <- data_dropna %>% arrange(HDI.Rank..2021.) %>% head(5)
bottom_5_countries <- data_dropna %>% arrange(desc(HDI.Rank..2021.)) %>% head(5)

# Step 3: Reshape data into long format for both top and bottom countries
# For top 5 countries
top_5_long <- top_5_countries %>%
  pivot_longer(cols = starts_with("Inequality.in.income"), 
               names_to = "Year", 
               values_to = "Inequality") %>%
  mutate(Year = sub("Inequality.in.income..", "", Year))

# For bottom 5 countries
bottom_5_long <- bottom_5_countries %>%
  pivot_longer(cols = starts_with("Inequality.in.income"), 
               names_to = "Year", 
               values_to = "Inequality") %>%
  mutate(Year = sub("Inequality.in.income..", "", Year))

# Step 4: Create the line plot for the top 5 and bottom 5 countries
ggplot() +
  geom_line(data = top_5_long, aes(x = Year, y = Inequality, group = Country, color = Country), size = 1) +
  geom_line(data = bottom_5_long, aes(x = Year, y = Inequality, group = Country, color = Country), size = 1) +
  labs(title = "Inequality in Income from 2010 to 2021 for Top 5 and Bottom 5 Countries by HDI Rank",
       x = "Year", y = "Inequality in Income") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = c("red", "blue", "green", "purple", "orange", "pink", "cyan", "brown", "yellow", "grey")) +
  guides(color = guide_legend(title = "Country"))

#Plot 6 
# Step 1: Group data by 'Human Development Groups' and calculate the mean for inequality columns
avg_inequality_by_group <- data %>%
  group_by(Human.Development.Groups) %>%
  summarise(across(starts_with("Inequality.in.income."), ~ mean(. , na.rm = TRUE)))

# Step 2: Reshape the data to a long format for plotting
avg_inequality_long <- avg_inequality_by_group %>%
  pivot_longer(cols = starts_with("Inequality.in.income."),
               names_to = "Year", values_to = "Average.Inequality") %>%
  mutate(Year = sub("Inequality.in.income..", "", Year))  # Clean up Year column names

# Step 3: Create the line plot
ggplot(avg_inequality_long, aes(x = Year, y = Average.Inequality, color = Human.Development.Groups, group = Human.Development.Groups)) +
  geom_line() +
  labs(title = "Average Inequality in Income from 2010 to 2021 by Human Development Group",
       x = "Year",
       y = "Average Inequality in Income") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_brewer(palette = "Set3") +  # Choose a color palette for groups
  guides(color = guide_legend(title = "Human Development Group"))

# Boxplots for Variability by Year ---------------------------------------

# Melt the data to a long format for easier plotting
data_long <- data %>%
  pivot_longer(cols = starts_with("Inequality"), names_to = "Year", values_to = "Inequality")

# Plot boxplots of Inequality for each year to observe changes in variability
boxplot_inequality <- ggplot(data_long, aes(x = Year, y = Inequality)) +
  geom_boxplot(fill = "lightgreen", color = "darkgreen") +
  theme_minimal() +
  ggtitle("Boxplot of Inequality in Income by Year") +
  xlab("Year") + ylab("Inequality in Income")
print(boxplot_inequality)

# Plots for Analyzing Linearity ------------------------------------------

# Scatter plots with linear and lowess trend lines for each year
inequality_years <- grep("Inequality", colnames(data), value = TRUE)

for (year in inequality_years) {
  plot <- ggplot(data, aes_string(x = "HDI.Rank..2021.", y = year)) +
    geom_point(color = "darkred") +
    geom_smooth(method = "lm", color = "blue", se = FALSE) +    # Linear trend line
    geom_smooth(method = "loess", color = "green", se = FALSE) + # Non-linear trend line
    ggtitle(paste("HDI Rank (2021) vs", year)) +
    xlab("HDI Rank (2021)") +
    ylab(year) +
    theme_minimal()
  
  print(plot)
}

#CORRELATION COMPUTING

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

# Final CSV exports
write.csv(inequality_long, "reshaped_inequality_data.csv", row.names = FALSE)

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

# Plot Pearson and Spearman correlations for Year vs HDI Rank

# Combine the Pearson and Spearman correlation results into one data frame
correlations_combined <- pearson_cor_year_hdi %>%
  left_join(spearman_cor_year_hdi, by = "Year") %>%
  gather(key = "Correlation_Type", value = "Correlation_Coefficient", pearson_corr, spearman_corr)

# Plot correlations
ggplot(correlations_combined, aes(x = Year, y = Correlation_Coefficient, color = Correlation_Type, group = Correlation_Type)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(title = "Pearson and Spearman Correlations between HDI Rank and Inequality by Year",
       x = "Year", y = "Correlation Coefficient") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = c("blue", "red"))  # Blue for Pearson, Red for Spearman

