# Load required libraries
library(readxl)
library(ggplot2)
library(cluster)
library(factoextra)
library(dplyr)
library(car)

# Load dataset
Smartwatch <- read_excel(file.choose("SmartWatch Data File.xlsx"))

# View dataset
View(Smartwatch)
names(Smartwatch)
summary(Smartwatch)

# Segmentation variables
segmentation_variables <- c("ConstCom", "TimelyInf", "TaskMgm", "DeviceSt", 
                            "Wellness", "Athlete", "Style", "AmznP",
                            "Female", "Degree", "Income", "Age")

df_segments <- Smartwatch[, segmentation_variables]

# STANDARDIZE DATA
df_scaled <- scale(df_segments)

#Optimal Clusters
set.seed(123)

# Elbow Method
fviz_nbclust(df_scaled, kmeans, method = "wss") +
  ggtitle("Elbow Method for Optimal Clusters")

#K-means Clustering
optimal_clusters <- 4
kmeans_result <- kmeans(df_scaled, centers = optimal_clusters, nstart = 25)

#Cluster labels in dataset
Smartwatch$Cluster <- kmeans_result$cluster

#Hierarchical Clustering
hc.w <- hclust(dist(df_scaled), method = 'ward.D2')
plot(hc.w, main = "Cluster Dendrogram", xlab = "Observations", ylab = "Height")
rect.hclust(hc.w, k = optimal_clusters, border = 2:5)

#Cluster Summary
cluster_summary <- Smartwatch %>%
  group_by(Cluster) %>%
  summarise(across(all_of(segmentation_variables), ~ mean(.x, na.rm = TRUE)))

print(cluster_summary, n = Inf, width = Inf)

#Plot Clusters
fviz_cluster(kmeans_result, data = df_scaled, ellipse.type = "norm") +
  ggtitle("K-means Clustering Results")

#Save
write.csv(Smartwatch, "Segmented_SmartWatch_Data.csv", row.names = FALSE)
