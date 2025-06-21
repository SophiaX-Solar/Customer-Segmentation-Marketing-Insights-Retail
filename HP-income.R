# Load libraries if not already loaded
library(dplyr)
library(ggplot2)

# Step 1: Clean income variable (Q288)
# Exclude missing or invalid codes like 77, 88, 99
data <- data %>% filter(Q288 >= 1 & Q288 <= 10)

# Convert income decile to labeled factor
income_labels <- paste("Decile", 1:10)
data$Income_Decile <- factor(data$Q288, levels = 1:10, labels = income_labels)

# Step 2: Create cross-tabulation table
income_table <- table(data$Income_Decile, data$DigitalUser)
print(income_table)

# Step 3: Check assumptions before Chi-square test
cat("Minimum observed frequency:", min(income_table), "\n")

# Step 4: Run Chi-square test
chi_income <- chisq.test(income_table)
expected_income <- round(chi_income$expected, 2)
print(expected_income)
cat("Minimum expected frequency:", min(expected_income), "\n")

# Step 5: Final Chi-square test result
print(chi_income)

# Step 6: Visualise distribution
ggplot(data, aes(x = Income_Decile, fill = DigitalUser)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Distribution of Digital Users by Household Income Decile",
       x = "Household Income Decile", y = "Proportion",
       fill = "User Type") +
  theme_minimal()

# Step 7: Optional - proportion table
prop.table(income_table, margin = 1)
