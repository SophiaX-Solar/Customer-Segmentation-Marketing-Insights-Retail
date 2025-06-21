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
library(ggplot2)     # 数据可视化
library(reshape2)    # 数据变形

# 📌 1️⃣ 读取数据
data <- read.csv("~/Desktop/Marketing Analysis/Assign1/SmartFresh Retail.csv")

# 计算 RFM 特征
data_clean <- data %>%
  mutate(
    F = Purchases_Online + Purchases_Catalog + Purchases_Store,  # 购买频率
    M = Spend_Wine + Spend_OrganicFood + Spend_Meat +
      Spend_WellnessProducts + Spend_Treats + Spend_LuxuryGoods,  # 总消费金额
    Avg_Spend = M / (F + 1)  # 平均单次消费金额，避免 M 右偏
  ) %>%
  select(-M)  # 移除 M，使用 Avg_Spend 替代

# 计算新变量
data_clean <- data_clean %>%
  mutate(
    Promo_Sensitivity = Promo_Purchases / (F + 1),  # 促销敏感度
    Luxury_Preference = Spend_LuxuryGoods / (Avg_Spend + 1)  # 高端产品偏好
  )

# 计算年龄（假设当前年份为 2025）
data_clean <- data_clean %>%
  mutate(Age = 2025 - Year_Birth)

# 去除缺失值
data_clean <- drop_na(data_clean)

# 📌 3️⃣ 选择用于 PCA 的变量
data_pca <- data_clean %>%
  select(Age, Annual_Income, F, Avg_Spend, Promo_Sensitivity, Luxury_Preference, 
         Purchases_Online, Purchases_Catalog, Purchases_Store, 
         Spend_WellnessProducts, Spend_Treats, Spend_LuxuryGoods)

# 📌 3.5️⃣ 处理离群点（95% 置信区间）
remove_outliers <- function(x) {
  q1 <- quantile(x, 0.025, na.rm = TRUE)
  q3 <- quantile(x, 0.975, na.rm = TRUE)
  x[x < q1] <- q1
  x[x > q3] <- q3
  return(x)
}


data_pca <- data_pca %>%
  mutate(across(where(is.numeric), ~ predict(BoxCoxTrans(.x), .x)))


data_pca <- data_pca %>%
  mutate(across(where(is.numeric), remove_outliers))

# 📌 4️⃣ 计算相关性矩阵，并可视化
cor_matrix <- cor(data_pca, use = "complete.obs")
melted_corr <- melt(cor_matrix)

# 相关性热力图，添加数值标签
ggplot(melted_corr, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  geom_text(aes(label=round(value, 2)), color="black", size=3) +
  scale_fill_gradient2(low="blue", high="red", mid="white", 
                       midpoint=0, limit=c(-1,1), space="Lab", 
                       name="Pearson Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

# 📌 5️⃣ 运行 PCA
data_scaled <- data_pca %>%
  mutate(across(where(is.numeric), scale))
pca_result <- PCA(data_scaled, graph = FALSE)

# 查看变量贡献
print(pca_result$var$contrib)

# 可视化变量贡献
fviz_pca_var(pca_result, col.var = "cos2", gradient.cols = c("blue", "red"))

# 解释方差
fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 50))

print(pca_result$var$contrib)


# 📌 6️⃣ 运行 K-Means 聚类
# 📌 7️⃣ 只使用前 3 个主成分进行 K-Means 聚类
reduced_pca <- as.data.frame(pca_result$ind$coord[, 1:3])  # 取前 3 个主成分

# 运行优化后的 K-Means 聚类
set.seed(123)
kmeans_result <- kmeans(reduced_pca, centers = 3, nstart = 50)

# 计算轮廓系数
silhouette_score <- silhouette(kmeans_result$cluster, dist(reduced_pca))

# 📌 8️⃣ 可视化 K-Means 结果
fviz_cluster(kmeans_result, data = reduced_pca, ellipse.type = "convex", geom = "point")

# 可视化轮廓系数
fviz_silhouette(silhouette_score)




data_pca$cluster <- factor(kmeans_result$cluster)
ggplot(data_pca, aes(x=cluster, y=Avg_Spend, fill=cluster)) + 
  geom_boxplot() + theme_minimal()
