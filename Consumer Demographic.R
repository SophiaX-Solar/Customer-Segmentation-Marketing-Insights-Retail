

library(ggplot2)
library(naniar)  
library(mice)
library(dplyr)
library(ggpubr)
library(tidyr)

data <- read.csv("~/Desktop/Marketing Analysis/Assign1/SmartFresh Retail.csv") # please update with the correct file path

#Step1:Delete the missing value row
data_clean <- data[!is.na(data$Annual_Income), ]
colSums(is.na(data_clean))

#Step2:Create Age Groups

# Calculate age based on birth year
data_clean$Age <- 2025 - data_clean$Year_Birth

# Create age groups (5-year intervals)
data_clean$Age_Group <- cut(
  data_clean$Age, 
  breaks = c(seq(15, 85, by = 5), Inf),  # 
  right = FALSE, 
  include.lowest = TRUE,
  labels = c(paste(seq(15, 80, by = 5), seq(20, 85, by = 5) - 1, sep = "-"), "85+")
)

# Aggregate total spending by age group
age_spending <- data_clean %>%
  group_by(Age_Group) %>%
  summarise(
    Spend_Wine = sum(Spend_Wine, na.rm=TRUE),
    Spend_Meat = sum(Spend_Meat, na.rm=TRUE),
    Spend_OrganicFood = sum(Spend_OrganicFood, na.rm=TRUE),
    Spend_WellnessProducts = sum(Spend_WellnessProducts, na.rm=TRUE),
    Spend_Treats = sum(Spend_Treats, na.rm=TRUE),
    Spend_LuxuryGoods = sum(Spend_LuxuryGoods, na.rm=TRUE)
  )


#Step 3 Visualization 
#Step 3.1 Total Spending by Age Group 
age_spending_long <- pivot_longer(age_spending, 
                                  cols = -Age_Group, 
                                  names_to = "Category", 
                                  values_to = "Total_Spend")

# Create a stacked bar chart
ggplot(age_spending_long, aes(x = Age_Group, y = Total_Spend, fill = Category)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Total Spending by Age Group and Product Category",
       x = "Age Group",
       y = "Total Spending (£)",
       fill = "Product Category") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Step 3.2 Annual Income Distribution by Age Group
ggplot(data_clean, aes(x = Age_Group, y = Annual_Income, fill = Age_Group)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Annual Income Distribution by Age Group",
       x = "Age Group",
       y = "Annual Income (£)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  




data_45_75 <- data_clean %>%
  filter(Age >= 45 & Age <= 75)

#check marital
table(categorical_data$Marital_Status)

# Count marital status distribution
marital_status_distribution <- data_45_75 %>%
  group_by(Marital_Status) %>%
  summarise(Count = n())

# Visualization of marital status
ggplot(marital_status_distribution, aes(x = Marital_Status, y = Count, fill = Marital_Status)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Marital Status Distribution (Ages 45-75)",
       x = "Marital Status",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Count education level distribution
education_distribution <- data_45_75 %>%
  group_by(Education_Level) %>%
  summarise(Count = n())

# Visualization of education level
ggplot(education_distribution, aes(x = Education_Level, y = Count, fill = Education_Level)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Education Level Distribution (Ages 45-75)",
       x = "Education Level",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Aggregate kid and teen counts
kid_teen_distribution <- data_45_75 %>%
  summarise(
    Avg_Kidhome = mean(Kidhome, na.rm = TRUE),
    Avg_Teenhome = mean(Teenhome, na.rm = TRUE)
  )

# Print results
print(kid_teen_distribution)

# Create data frame for visualization
kid_teen_long <- tidyr::pivot_longer(kid_teen_distribution, 
                                     cols = c("Avg_Kidhome", "Avg_Teenhome"), 
                                     names_to = "Category", 
                                     values_to = "Average_Count")
# Aggregate counts for heatmap
heatmap_data <- data_45_75 %>%
  group_by(Education_Level, Marital_Status) %>%
  summarise(Count = n()) %>%
  ungroup()


#Step 3.3Heatmap of Marital Status vs. Education Level (Ages 45-75)
# Create heatmap
ggplot(heatmap_data, aes(x = Marital_Status, y = Education_Level, fill = Count)) +
  geom_tile(color = "white") +  # Create heatmap tiles
  scale_fill_gradient(low = "lightblue", high = "darkblue") +  # Set heatmap color scale
  theme_minimal() +
  labs(title = "Heatmap of Marital Status vs. Education Level (Ages 45-75)",
       x = "Marital Status",
       y = "Education Level",
       fill = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

#Step3.4  45-75 househod
kid_teen_analysis <- data_45_75 %>%
  group_by(Marital_Status) %>%
  summarise(
    Avg_chihome = mean(Kidhome+Teenhome, na.rm = TRUE),
  )

print(kid_teen_analysis)



