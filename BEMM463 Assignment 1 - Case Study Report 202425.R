library(readxl)
library(ggplot2)
library(cluster)
library(factoextra)
library(dplyr)
library(car)

Smartwatch <- read_excel(file.choose("SmartWatch Data File.xlsx"))
View(Smartwatch)
names(Smartwatch)
summary(Smartwatch)

segmentation_variables <- c("ConstCom", "TimelyInf", "TaskMgm", "DeviceSt", 
                       "Wellness", "Athlete", "Style","AmznP","Female","Degree","Income","Age")

df_segments <- Smartwatch[, segmentation_variables]

# STANDARDISE DATA
df_scaled <- scale(df_segments)

# Optimal Clusters
#Elbow Method
fviz_nbclust(df_scaled, kmeans, method = "wss") +
  ggtitle("Elbow Method for Optimal Clusters")

# 2. Clustering
hc.w <- hclust(dist(df_scaled), method = 'ward.D2')
Smartwatch$Cluster <- kmeans_result$cluster
plot(hc.w, main = "Cluster Dendrogram", xlab = "Observations", ylab = "Height")
rect.hclust(hc.w, k = 4, border = 2:5)

cluster_summary <- Smartwatch %>%
  group_by(Cluster) %>%
  summarise(across(all_of(segmentation_variables), ~ mean(.x, na.rm = TRUE)))

print(cluster_summary, n = Inf, width = Inf)

# PCA plot for cluster visualization
fviz_cluster(kmeans_result, data = df_scaled, ellipse.type = "norm") +
  ggtitle("K-means Clustering Results")

# Save the final dataset with cluster assignments
write.csv(Smartwatch, "Segmented_SmartWatch_Data.csv", row.names = FALSE)
