library(dplyr)
library(ggplot2)
library(tidyr)
library(stats)


#读取数据 (替换为你的文件路径)
data <- read.csv("/Users/xifei/Desktop/Marketing Analysis/Assign1/SmartFresh Retail.csv") # 请替换为正确的文件路径
data_clean <- data[!is.na(data$Annual_Income), ]
colSums(is.na(data_clean))

data_clean$Age <- 2025 - data_clean$Year_Birth

df <- data_clean %>%
  filter(Age >= 45 & Age <= 75)


# 选择消费行为变量
spending_vars <- c("Spend_Wine", "Spend_LuxuryGoods", "Spend_OrganicFood", 
                   "Spend_Meat", "Spend_WellnessProducts", "Spend_Treats",
                   "Promo_Purchases", "Response_Latest",
                   "Purchases_Online", "Purchases_Store", "Purchases_Catalog")

# **去除离群点函数（IQR 方法）**
remove_outliers_iqr <- function(df, variable) {
  Q1 <- quantile(df[[variable]], 0.25, na.rm = TRUE)
  Q3 <- quantile(df[[variable]], 0.75, na.rm = TRUE)
  IQR_value <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value
  df <- df %>% filter(df[[variable]] >= lower_bound & df[[variable]] <= upper_bound)
  return(df)
}

# **对所有消费变量应用 IQR 方法**
for (var in spending_vars) {
  df <- remove_outliers_iqr(df, var)
}

# **筛选 Married 和 Together 组**
df_filtered <- df %>% filter(Marital_Status %in% c("Married", "Together"))

# **计算均值和标准差**
summary_stats <- df_filtered %>%
  group_by(Marital_Status) %>%
  summarise(across(all_of(spending_vars), list(mean = mean, sd = sd), na.rm = TRUE))

print(summary_stats)

# **可视化原始数据的 Wine Spending 分布**
ggplot(df_filtered, aes(x = Spend_Wine, fill = Marital_Status)) +
  geom_histogram(aes(y = ..density..), bins = 30, alpha = 0.5, position = "identity") +
  geom_density(alpha = 0.7) +
  theme_minimal() +
  labs(title = "Wine Spending Distribution: Married vs. Together",
       x = "Wine Spending (£)", y = "Density")

# **对数变换 (log1p: log(x + 1) 避免 0 值问题)**
df_filtered <- df_filtered %>% mutate(Log_Spend_Wine = log1p(Spend_Wine))

# **可视化 Log 变换后的数据分布**
ggplot(df_filtered, aes(x = Log_Spend_Wine, fill = Marital_Status)) +
  geom_histogram(aes(y = ..density..), bins = 30, alpha = 0.5, position = "identity") +
  geom_density(alpha = 0.7) +
  theme_minimal() +
  labs(title = "Log-Transformed Wine Spending Distribution: Married vs. Together",
       x = "Log(Wine Spending + 1)", y = "Density")

# **检查正态性（Shapiro-Wilk 检验）**
shapiro.test(df_filtered$Log_Spend_Wine[df_filtered$Marital_Status == "Married"])
shapiro.test(df_filtered$Log_Spend_Wine[df_filtered$Marital_Status == "Together"])

# **如果数据仍不正态，执行 Mann-Whitney U 检验（Wilcoxon Rank-Sum Test）**
wilcox.test(Log_Spend_Wine ~ Marital_Status, data = df_filtered)

# **如果两组数据符合正态分布，执行 T 检验**
t.test(Log_Spend_Wine ~ Marital_Status, data = df_filtered, var.equal = FALSE)
