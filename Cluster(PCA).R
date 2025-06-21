# Load necessary R packages
library(tidyverse)
library(lubridate)
library(factoextra)  # PCA visualization
library(FactoMineR)  # PCA computation
library(corrplot)    # Correlation analysis
library(cluster)     # Calculate silhouette coefficient
library(caret)       # One-Hot encoding
library(e1071)       # Skewness calculation
library(DescTools)   # Winsorization for extreme value handling
library(car)         # Box-Cox transformation
library(ggplot2)     # Data visualization
library(reshape2)    # Data reshaping

# Read data
df <- read.csv("~/Desktop/Marketing Analysis/Assign1/SmartFresh Retail.csv")
data <- df %>% drop_na(Annual_Income)

# Compute RFM features
data_clean <- data %>%
  mutate(
    F = Purchases_Online + Purchases_Catalog + Purchases_Store,  # Purchase frequency
    M = Spend_Wine + Spend_OrganicFood + Spend_Meat +
      Spend_WellnessProducts + Spend_Treats + Spend_LuxuryGoods,  # Total spend amount
    Avg_Spend = M / (F + 1)  # Average spend per purchase, avoiding right skew of M
  ) %>%
  select(-M)  # Remove M, using Avg_Spend instead

# Compute new variables
data_clean <- data_clean %>%
  mutate(
    Promo_Sensitivity = Promo_Purchases / (F + 1),  # Sensitivity to promotions
    Luxury_Preference = Spend_LuxuryGoods / (Avg_Spend + 1)  # Preference for luxury products
  )

# Calculate age (assuming current year is 2025)
data_clean <- data_clean %>%
  mutate(Age = 2025 - Year_Birth)

# Remove missing values
data_clean <- drop_na(data_clean)

#Select variables for PCA
data_pca <- data_clean %>%
  select(Age, Annual_Income, F, Avg_Spend, Promo_Sensitivity, Luxury_Preference, 
         Purchases_Online, Purchases_Catalog, Purchases_Store, 
         Spend_WellnessProducts, Spend_Treats, Spend_LuxuryGoods)

# Handle outliers (95% confidence interval)
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

# Compute and visualize correlation matrix
cor_matrix <- cor(data_pca, use = "complete.obs")
melted_corr <- melt(cor_matrix)

# Correlation heatmap with numerical labels
ggplot(melted_corr, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  geom_text(aes(label=round(value, 2)), color="black", size=3) +
  scale_fill_gradient2(low="blue", high="red", mid="white", 
                       midpoint=0, limit=c(-1,1), space="Lab", 
                       name="Pearson Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

# Run PCA
data_scaled <- data_pca %>%
  mutate(across(where(is.numeric), scale))
pca_result <- PCA(data_scaled, graph = FALSE)

# Check variable contributions
print(pca_result$var$contrib)

# Visualize variable contributions
fviz_pca_var(pca_result, col.var = "cos2", gradient.cols = c("blue", "red"))

# Explained variance
fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 50))



#  Run K-Means clustering
# Use only first 2 principal components for K-Means clustering
reduced_pca <- as.data.frame(pca_result$ind$coord[, 1:2])  # Use first 2 principal components

# Run optimized K-Means clustering
set.seed(123)
kmeans_result <- kmeans(reduced_pca, centers = 3, nstart = 50)

# Compute silhouette scores
silhouette_score <- silhouette(kmeans_result$cluster, dist(reduced_pca))

# Visualize K-Means results
fviz_cluster(kmeans_result, data = reduced_pca, ellipse.type = "convex", geom = "point")

# Visualize silhouette scores
fviz_silhouette(silhouette_score)

original_data <- data_pca
original_data$cluster <- factor(kmeans_result$cluster)

cluster_summary <- original_data %>%
  group_by(cluster) %>%
  summarise(
    Avg_Age = mean(Age),
    Avg_Annual_Income = mean(Annual_Income),
    Avg_Frequency = mean(F),
    Avg_Spend = mean(Avg_Spend),
    Promo_Sensitivity = mean(Promo_Sensitivity),
    Luxury_Preference = mean(Luxury_Preference),
    Online_Purchases = mean(Purchases_Online),
    Catalog_Purchases = mean(Purchases_Catalog),
    Store_Purchases = mean(Purchases_Store),
    Wellness_Spend = mean(Spend_WellnessProducts),
    Treats_Spend = mean(Spend_Treats),
    Luxury_Spend = mean(Spend_LuxuryGoods)
  )

print(cluster_summary)
library(fmsb)

# Prepare data for Radar Plot
radar_data <- cluster_summary %>%
  column_to_rownames("cluster") %>%
  as.data.frame()

# Normalize data to [0,1] for better visualization
radar_data_norm <- as.data.frame(lapply(radar_data, scales::rescale))

# Add max-min rows (required by fmsb)
radar_data_norm <- rbind(rep(1, ncol(radar_data_norm)), rep(0, ncol(radar_data_norm)), radar_data_norm)

# Plot radar chart
colors <- c("red", "green", "blue")
radarchart(radar_data_norm, axistype=1, 
           pcol=colors, pfcol=scales::alpha(colors, 0.4), plwd=2, plty=1, 
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,1,0.2), cglwd=0.8, 
           vlcex=0.8)

legend(x="topright", legend=paste("Cluster", 1:3), col=colors, lty=1, lwd=2, bty="n")

