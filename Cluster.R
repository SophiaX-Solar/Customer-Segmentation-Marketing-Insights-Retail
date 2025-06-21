# 加载 R 语言的必要包
library(tidyverse)
library(lubridate)
library(factoextra)  # PCA 可视化
library(FactoMineR)  # 计算 PCA
library(corrplot)    # 相关性分析
library(cluster)     # 计算轮廓系数
library(caret)       # One-Hot 编码
library(e1071)       # 偏态计算
library(DescTools)   # Winsorization 处理极端值
library(car)         # Box-Cox 变换

# 📌 1️⃣ 读取数据
data <- read.csv("~/Desktop/Marketing Analysis/Assign1/SmartFresh Retail.csv")

# 计算 RFM 特征
data_clean <- data %>%
  mutate(
    R = Last_Interaction,  # 最近一次交互的天数
    F = Purchases_Online + Purchases_Catalog + Purchases_Store,  # 总购买次数
    M = Spend_Wine + Spend_OrganicFood + Spend_Meat +
      Spend_WellnessProducts + Spend_Treats + Spend_LuxuryGoods  # 总消费金额
  )

# 计算新变量
data_clean <- data_clean %>%
  mutate(
    Promo_Sensitivity = Promo_Purchases / (F + 1),  # 促销敏感度
    Luxury_Preference = Spend_LuxuryGoods / (M + 1),  # 高端产品偏好
    Online_Ratio = Purchases_Online / (F + 1),  # 线上渠道占比
    Catalog_Ratio = Purchases_Catalog / (F + 1),  # 目录渠道占比
    Store_Ratio = Purchases_Store / (F + 1)  # 线下渠道占比
  )

# 计算年龄（假设当前年份为 2025）
data_clean <- data_clean %>%
  mutate(Age = 2025 - Year_Birth)

# 去除缺失值
data_clean <- drop_na(data_clean)

# 📌 2️⃣ 处理分类变量（One-Hot Encoding）
dummy_vars <- dummyVars(~ Education_Level + Marital_Status, data = data_clean, fullRank = FALSE)
data_encoded <- predict(dummy_vars, data_clean) %>% as.data.frame()
data_clean <- cbind(data_clean, data_encoded) %>%
  select(-Education_Level, -Marital_Status)

# 📌 3️⃣ 指定用于 PCA 的变量
data_pca <- data_clean %>%
  select(R, F, M)

# 📌 4️⃣ 替换离群点（95% 置信区间）
replace_outliers <- function(df) {
  df %>%
    mutate(across(where(is.numeric), ~ ifelse(
      . > quantile(., 0.975, na.rm = TRUE), quantile(., 0.975, na.rm = TRUE),
      ifelse(. < quantile(., 0.025, na.rm = TRUE), quantile(., 0.025, na.rm = TRUE), .)
    )))
}

data_pca <- replace_outliers(data_pca)

# 📌 5️⃣ 计算所有数值变量的偏态
skewness_values <- data_pca %>%
  summarise(across(where(is.numeric), ~ skewness(.x, na.rm = TRUE)))

# 打印偏态值
print(skewness_values)

# 筛选偏态值大于 1 的变量
skewed_vars <- names(which(skewness_values > 1))

# 使用 Box-Cox 变换
for (var in skewed_vars) {
  bc_trans <- BoxCoxTrans(data_pca[[var]])
  data_pca[[var]] <- predict(bc_trans, data_pca[[var]])
}

# 确保数据完整
data_pca <- na.omit(data_pca)

# 📌 6️⃣ 计算并打印相关性矩阵
cor_matrix <- cor(data_pca, use = "complete.obs")
print(cor_matrix)  # 打印相关性系数
corrplot(cor_matrix, method = 'ellipse', order = "hclust")

# 📌 7️⃣ 重新标准化
data_scaled <- data_pca %>%
  mutate(across(where(is.numeric), scale)) %>%
  na.omit()

# 检查数据维度
dim(data_scaled)

# 📌 8️⃣ 运行 PCA
pca_result <- PCA(data_scaled, graph = FALSE)

# 查看变量贡献
print(pca_result$var$contrib)

# 可视化变量贡献
fviz_pca_var(pca_result, col.var = "cos2", gradient.cols = c("blue", "red"))

# 解释方差
fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 50))

# 📌 9️⃣ 运行 K-Means 聚类
set.seed(123)
kmeans_result <- kmeans(data_scaled, centers = 3, nstart = 25)

# 聚类可视化
fviz_cluster(kmeans_result, data = data_scaled, ellipse.type = "convex")

# 计算轮廓系数
silhouette_score <- silhouette(kmeans_result$cluster, dist(data_scaled))
fviz_silhouette(silhouette_score)

