# Install the required libraries if not already installed
install.packages("ggplot2")
install.packages("factoextra")
install.packages("dplyr")

install.packages("colorspace")
# Load the required libraries
library(ggplot2)
library(factoextra)
library(dplyr)

#Load the Mall Customers Dataset
mall_customers = read.csv(D:\MIT\Semester-7\Big Data Analytics Lab\All labs\Lab-4\Mall_Customers.csv")
head(mall_customers)

names(mall_customers)

print(colnames(mall_customers))
data <- mall_customers[c("AnnualIncome","SpendingScore")]
head(data)

# Scale the data (optional, but recommended for K-Means)
data_scaled <- scale(data)
print("scaled data")
head(data_scaled)
# calculated WSS for each clustes from 1 to 15
wss <- numeric(15)
for (k in 1:15) wss[k] <- sum(kmeans(data_scaled, centers=k,nstart=25)$withinss)
# plot the graph of number of clusters vs WSS
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="WSS")
# Elbow method to find the optimal number of clusters
fviz_nbclust(data_scaled, kmeans, method = "wss") + 
  geom_vline(xintercept = 5, linetype = 2) +
  labs(subtitle = "Elbow method")


# Silhouette method
fviz_nbclust(data_scaled, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")
  
# Apply K-Means clustering with 3 clusters
set.seed(123)  # Set seed for reproducibility in case python- random_state
kmeans_result <- kmeans(data_scaled, centers = 5, nstart = 25)

# Add the cluster assignments to the original dataset
mall_customers$Cluster <- as.factor(kmeans_result$cluster)
tail(mall_customers)

# Scatter plot of the clusters
ggplot(mall_customers, aes(x = AnnualIncome, y = SpendingScore, color = Cluster)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("red", "blue", "green","yellow","black")) +
  labs(title = "K-Means Clustering of Mall Customers",
       x = "Annual Income (k$)",
       y = "Spending Score (1-100)") +
  theme_minimal()

# Scatter plot with cluster centers
fviz_cluster(kmeans_result, data = data_scaled,
             geom = "point",
             ellipse.type = "norm",
             ggtheme = theme_minimal(),
             palette = c("red", "blue", "green","yellow","black"))
kmeans_result