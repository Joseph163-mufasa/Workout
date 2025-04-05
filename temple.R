# Load necessary libraries
library(tidyverse)
library(scales)

# Step 1: Import Data
# Replace with your actual dataset
data <- read.csv("kenya_socioeconomic_data.csv")

# Step 2: Data Wrangling with tidyverse
# Check the structure of the dataset
glimpse(data)

# Clean column names
data <- data %>%
  janitor::clean_names()  # Ensure consistent column naming

# Filter rows and select relevant columns
data_filtered <- data %>%
  filter(!is.na(population), !is.na(literacy_rate), !is.na(gdp_contribution)) %>%
  select(county, population, literacy_rate, healthcare_access, gdp_contribution)

# Create new variables (e.g., GDP per capita, population density)
data <- data %>%
  mutate(gdp_per_capita = gdp_contribution / population,
         literacy_category = case_when(
           literacy_rate >= 0.8 ~ "High",
           literacy_rate >= 0.5 ~ "Medium",
           TRUE ~ "Low"
         ))

# Step 3: Exploratory Data Analysis (EDA)
# Summary statistics
data_summary <- data %>%
  summarise(
    avg_population = mean(population, na.rm = TRUE),
    avg_gdp_per_capita = mean(gdp_per_capita, na.rm = TRUE),
    avg_literacy_rate = mean(literacy_rate, na.rm = TRUE)
  )

print(data_summary)

# Correlation analysis
cor_matrix <- data %>%
  select(population, literacy_rate, healthcare_access, gdp_contribution) %>%
  cor(use = "complete.obs")

print(cor_matrix)

# Step 4: Data Visualization
# Population vs GDP contribution
ggplot(data, aes(x = population, y = gdp_contribution, color = literacy_category)) +
  geom_point(size = 3, alpha = 0.7) +
  scale_x_log10(labels = comma) +
  scale_y_log10(labels = dollar) +
  labs(
    title = "Population vs GDP Contribution Across Counties",
    x = "Population (Log Scale)",
    y = "GDP Contribution (Log Scale)"
  ) +
  theme_minimal()

# Literacy rate distribution
ggplot(data, aes(x = literacy_rate, fill = literacy_category)) +
  geom_histogram(binwidth = 0.05, color = "black", alpha = 0.8) +
  scale_x_continuous(labels = percent_format()) +
  labs(
    title = "Distribution of Literacy Rates",
    x = "Literacy Rate",
    y = "Number of Counties"
  ) +
  theme_minimal()

# GDP per capita and healthcare access
ggplot(data, aes(x = healthcare_access, y = gdp_per_capita, color = literacy_category)) +
  geom_jitter(size = 3, alpha = 0.7) +
  scale_y_continuous(labels = dollar) +
  labs(
    title = "GDP Per Capita vs Healthcare Access",
    x = "Healthcare Access (%)",
    y = "GDP Per Capita"
  ) +
  theme_minimal()

# Step 5: Save Cleaned Dataset and Outputs
write.csv(data, "cleaned_kenya_socioeconomic_data.csv", row.names = FALSE)

# Export visualizations
ggsave("Population_vs_GDP_Contribution.png", width = 8, height = 6)
ggsave("Literacy_Rate_Distribution.png", width = 8, height = 6)
ggsave("GDP_Per_Capita_vs_Healthcare_Access.png", width = 8, height = 6)
