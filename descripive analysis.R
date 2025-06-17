library(ggplot2)
library(naniar)  
library(mice)
library(dplyr)
library(ggpubr)
library(tidyr)

data <- read.csv("～/Desktop/Marketing Analysis/Assign1/SmartFresh Retail.csv") # please update with the correct file path

#Step1:Delete the missing value row
data_clean <- data[!is.na(data$Annual_Income), ]
colSums(is.na(data_clean))

# Step 2: Create Age Groups
# Calculate age based on birth year
data_clean$Age <- 2025 - data_clean$Year_Birth

# Create age groups (5-year intervals)
data_clean$Age_Group <- cut(data_clean$Age, breaks=seq(20, 100, by=5), right=FALSE)

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

#Step 3.1- Total Spending by Age Group 
# Convert data to long format for visualization
library(tidyr)
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

# most of customers are 45-65 and outliners

#check categorical data
categorical_data <- data_clean %>% select(where(is.character))
sapply(categorical_data, function(x) length(unique(x)))

#check marital
table(data$Marital_Status)

#to check the average spend of "Together", "Married"
data %>%
  filter(Marital_Status %in% c("Together", "Married")) %>%  
  group_by(Marital_Status) %>%
  summarise(Average_Spend = mean(Spend_Wine + Spend_OrganicFood + Spend_Meat +
                                   Spend_WellnessProducts + Spend_Treats + Spend_LuxuryGoods, na.rm = TRUE))


#spend distirbution right skewness
ggplot(data, aes(x = Spend_Wine + Spend_OrganicFood + Spend_Meat + 
                   Spend_WellnessProducts + Spend_Treats + Spend_LuxuryGoods)) +
  geom_histogram(binwidth = 100, fill = "blue", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Total Spending Distribution", x = "Total Spending (£)", y = "Count")

#wilcox to check this two type
wilcox.test(
  (Spend_Wine + Spend_OrganicFood + Spend_Meat +
     Spend_WellnessProducts + Spend_Treats + Spend_LuxuryGoods) ~ Marital_Status, 
  data = data %>% filter(Marital_Status %in% c("Together", "Married"))
)

# box-plot
ggplot(data %>% filter(Marital_Status %in% c("Together", "Married")), 
       aes(x = Marital_Status, 
           y = Spend_Wine + Spend_OrganicFood + Spend_Meat + 
             Spend_WellnessProducts + Spend_Treats + Spend_LuxuryGoods, 
           fill = Marital_Status)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  labs(title = "Comparison of Total Spending: Together vs Married",
       x = "Marital Status", y = "Total Spending (£)")

#data$Marital_Status <- recode(data$Marital_Status, "Together" = "Married")


ggplot(data %>% filter(Marital_Status %in% c("Single", "YOLO",'Absurd','Alone','Divorced','Widow')), 
       aes(x = Marital_Status, 
           y = Spend_Wine + Spend_OrganicFood + Spend_Meat + 
             Spend_WellnessProducts + Spend_Treats + Spend_LuxuryGoods, 
           fill = Marital_Status)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  labs(title = "Comparison of Total Spending: Together vs Married",
       x = "Marital Status", y = "Total Spending (£)")


#merge YOLO &Single ？  
#data$Marital_Status <- recode(data$Marital_Status, "YOLO" = "Single")

# delete "Absurd"
data <- data %>%
  filter(!(Marital_Status %in% c("Absurd", "YOLO", "Alone")))

table(data$Marital_Status)



ggplot(data %>% filter(Marital_Status %in% c('Divorced','Married',"Single",'Alone',)), 
       aes(x = Marital_Status, 
           y = Spend_Wine + Spend_OrganicFood + Spend_Meat + 
             Spend_WellnessProducts + Spend_Treats + Spend_LuxuryGoods, 
           fill = Marital_Status)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  labs(title = "Comparison of Total Spending: Together vs Married",
       x = "Marital Status", y = "Total Spending (£)")



data %>%
  group_by(Marital_Status) %>%
  summarise(
    Avg_Spend_Wine = mean(Spend_Wine, na.rm = TRUE),
    Avg_Spend_Organic = mean(Spend_OrganicFood, na.rm = TRUE),
    Avg_Spend_Meat = mean(Spend_Meat, na.rm = TRUE),
    Avg_Spend_Wellness = mean(Spend_WellnessProducts, na.rm = TRUE),
    Avg_Spend_Treats = mean(Spend_Treats, na.rm = TRUE),
    Avg_Spend_Luxury = mean(Spend_LuxuryGoods, na.rm = TRUE)
  ) %>%
  arrange(desc(Avg_Spend_Luxury))


ggplot(data, aes(x = Marital_Status, y = Spend_OrganicFood, fill = Marital_Status)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Luxury Goods Spending by Marital Status",
       x = "Marital Status", y = "Luxury Goods Spending (£)")




category_spending <- data %>%
  group_by(Marital_Status) %>%
  summarise(
    Avg_Spend_Wine = mean(Spend_Wine, na.rm = TRUE),
    Avg_Spend_OrganicFood = mean(Spend_OrganicFood, na.rm = TRUE),
    Avg_Spend_Meat = mean(Spend_Meat, na.rm = TRUE),
    Avg_Spend_Wellness = mean(Spend_WellnessProducts, na.rm = TRUE),
    Avg_Spend_Treats = mean(Spend_Treats, na.rm = TRUE),
    Avg_Spend_Luxury = mean(Spend_LuxuryGoods, na.rm = TRUE)
  ) %>%
  arrange(desc(Avg_Spend_Luxury))

print(category_spending)
exists("category_spending") 




category_spending_long <- category_spending %>%
  pivot_longer(cols = starts_with("Avg_Spend"), 
               names_to = "Product_Type", 
               values_to = "Average_Spending")

ggplot(category_spending_long, aes(x = Marital_Status, y = Average_Spending, fill = Product_Type)) +
  geom_bar(stat = "identity", position = "stack") +
  theme_minimal() +
  labs(title = "Average Spending on Different Products by Marital Status",
       x = "Marital Status", y = "Average Spending (£)", fill = "Product Type")

