library(dplyr)
library(ggplot2)
library(car)  # For Levene's Test

# Load the dataset
setwd("~/Desktop/Marketing Analysis/")
data <- read.csv("Assign2/WVS/WVS_Cross-National_Wave_7_csv_v6_0.csv")

# Create DigitalUser grouping variable based on Q206 and Q207
data$DigitalUser <- ifelse(data$Q206 %in% c(1, 2) | data$Q207 %in% c(1, 2), 1, 0)
data$DigitalUser <- factor(data$DigitalUser, levels = c(0, 1),
                           labels = c("Non-Digital User", "Digital User"))

# Descriptive statistics for Age (Q262)
data %>%
  group_by(DigitalUser) %>%
  summarise(
    Mean_Age = mean(Q262, na.rm = TRUE),
    SD_Age = sd(Q262, na.rm = TRUE),
    Median_Age = median(Q262, na.rm = TRUE),
    Count = n()
  )

# Normality test using Shapiro-Wilk on random samples
set.seed(42)
shapiro.test(sample(data$Q262[data$DigitalUser == "Digital User"], 5000))
shapiro.test(sample(data$Q262[data$DigitalUser == "Non-Digital User"], 5000))

# Homogeneity of variance test (Levene's Test)
leveneTest(Q262 ~ DigitalUser, data = data)

# Independent samples t-test (Welch's t-test due to unequal variances)
t.test(Q262 ~ DigitalUser, data = data, var.equal = FALSE)


