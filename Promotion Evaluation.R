# Load necessary libraries
library(ggplot2)
library(dplyr)
library(bestNormalize)  # Ensure bestNormalize package is installed

# Read the dataset
data <- read.csv("~/Desktop/Marketing Analysis/Assign1/SmartFresh Retail.csv")

#1 ilter users who did not accept any promotional offers**
df_no_promo <- data %>%
  filter(
    !(Accepted_Offer1 == 1 | Accepted_Offer2 == 1 | Accepted_Offer3 == 1 | 
        Accepted_Offer4 == 1 | Accepted_Offer5 == 1 | Response_Latest == 1)
  ) %>%
  filter(!is.na(Promo_Purchases), !is.na(Spend_Wine))

#2 Identify whether these users made purchases during the promotion period
df_no_promo <- df_no_promo %>%
  mutate(Purchase_Timing = ifelse(Promo_Purchases > 0, "During Promo", "Non-Promo"))

# ---- 3 Remove Outliers ----
# Calculate IQR (Interquartile Range)
Q1 <- quantile(df_no_promo$Spend_Wine, 0.25, na.rm = TRUE)
Q3 <- quantile(df_no_promo$Spend_Wine, 0.75, na.rm = TRUE)
IQR_value <- Q3 - Q1

# Define outlier boundaries
lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

# Filter out outliers
df_clean <- df_no_promo %>%
  filter(Spend_Wine >= lower_bound & Spend_Wine <= upper_bound)

# ---- ️4 Yeo-Johnson Transformation ----
# Perform Yeo-Johnson transformation for normality correction
yj_transform <- bestNormalize(df_clean$Spend_Wine, method = "yeo_johnson")

# Apply transformation to data
df_clean <- df_clean %>%
  mutate(Spend_Wine_YJ = predict(yj_transform))

# ---- 5️ Perform Normality Test ----
shapiro_test_result <- shapiro.test(df_clean$Spend_Wine_YJ)
print(shapiro_test_result)

# ---- 6️ Conduct T-test ----
t_test_result <- t.test(Spend_Wine_YJ ~ Purchase_Timing, data = df_clean, var.equal = FALSE)
print(t_test_result)


# Save the t-test output to a text file
sink("t_test_results.txt")  # Redirect output to file
print(t_test_result)        # Print t-test results
sink()                      # Close file connection



# Density plot of raw spending data
ggplot(df_clean, aes(x = Spend_Wine, fill = Purchase_Timing)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Raw Wine Spending",
       x = "Wine Spending (£)",
       y = "Density",
       fill = "Purchase Timing") +
  theme_minimal()
