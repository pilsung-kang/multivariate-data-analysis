# Package for cluster validity
install.packages("clValid")
install.packages("plotrix")

library(clValid)
library(plotrix)

# Part 1: K-Means Clustering ----------------------------------------------
# Load the Wine dataset
wine <- read.csv("wine.csv")

# Remove the class label
wine_class <- wine[,1]
wine_x <- wine[,-1]

# data scaling
wine_x_scaled <- scale(wine_x, center = TRUE, scale = TRUE)

# Evaluating the cluster validity measures
wine_clValid <- clValid(wine_x_scaled, 2:10, clMethods = "kmeans", 
                           validation = c("internal", "stability"))
summary(wine_clValid)

# Perform K-Means Clustering with the best K determined by Silhouette
wine_kmc <- kmeans(wine_x_scaled,3)

str(wine_kmc)
wine_kmc$centers
wine_kmc$size
wine_kmc$cluster

# Compare the cluster info. and class labels
real_class <- wine_class
kmc_cluster <- wine_kmc$cluster
table(real_class, kmc_cluster)

# Compare each cluster for KMC
cluster_kmc <- data.frame(wine_x_scaled, clusterID = as.factor(wine_kmc$cluster))
kmc_summary <- data.frame()

for (i in 1:(ncol(cluster_kmc)-1)){
  kmc_summary = rbind(kmc_summary, 
                     tapply(cluster_kmc[,i], cluster_kmc$clusterID, mean))
}

colnames(kmc_summary) <- paste("cluster", c(1:3))
rownames(kmc_summary) <- colnames(wine_x)
kmc_summary

# Radar chart
par(mfrow = c(1,3))
for (i in 1:3){
  plot_title <- paste("Radar Chart for Cluster", i, sep=" ")
  radial.plot(kmc_summary[,i], labels = rownames(kmc_summary), 
              radial.lim=c(-2,2), rp.type = "p", main = plot_title, 
              line.col = "red", lwd = 3, show.grid.labels=1)
}
dev.off()

# Compare the first and the second cluster
kmc_cluster1 <- wine_x[wine_kmc$cluster == 1,]
kmc_cluster2 <- wine_x[wine_kmc$cluster == 2,]

# t_test_result
kmc_t_result <- data.frame()

for (i in 1:13){
  
  kmc_t_result[i,1] <- t.test(kmc_cluster1[,i], kmc_cluster2[,i], 
                              alternative = "two.sided")$p.value
  
  kmc_t_result[i,2] <- t.test(kmc_cluster1[,i], kmc_cluster2[,i], 
                              alternative = "greater")$p.value
  
  kmc_t_result[i,3] <- t.test(kmc_cluster1[,i], kmc_cluster2[,i], 
                              alternative = "less")$p.value
}

kmc_t_result

# Part 2: Hierarchical Clustering -----------------------------------------
ploan <- read.csv("Personal Loan.csv")
ploan_x <- ploan[,-c(1,5,10)]
ploan_x_scaled <- scale(ploan_x, center = TRUE, scale = TRUE)

# Compute the similarity using the spearman coefficient
cor_Mat <- cor(t(ploan_x_scaled), method = "spearman")
dist_ploan <- as.dist(1-cor_Mat)

# Perform hierarchical clustering
hr <- hclust(dist_ploan, method = "complete", members=NULL)

# plot the results
plot(hr)
plot(hr, hang = -1)
plot(as.dendrogram(hr), edgePar=list(col=3, lwd=4), horiz=T)

# Find the clusters
mycl <- cutree(hr, k=10)
mycl

plot(hr)
rect.hclust(hr, k=10, border="red")


# Compare each cluster for HC
ploan_hc <- data.frame(ploan_x_scaled, ploanYN = ploan[,10], 
                         clusterID = as.factor(mycl))
hc_summary <- data.frame()

for (i in 1:(ncol(ploan_hc)-1)){
  hc_summary = rbind(hc_summary, 
                     tapply(ploan_hc[,i], ploan_hc$clusterID, mean))
}

colnames(hc_summary) <- paste("cluster", c(1:10))
rownames(hc_summary) <- c(colnames(ploan_x), "LoanRatio")
hc_summary

# Radar chart
par(mfrow = c(2,5))
for (i in 1:10){
  plot_title <- paste("Radar Chart for Cluster", i, sep=" ")
  radial.plot(hc_summary[,i], labels = rownames(hc_summary), 
              radial.lim=c(-2,2), rp.type = "p", main = plot_title, 
              line.col = "red", lwd = 3, show.grid.labels=1)
}
dev.off()

# Compare the cluster 7 & 8
hc_cluster7 <- ploan_hc[ploan_hc$clusterID == 7, c(1:11)]
hc_cluster8 <- ploan_hc[ploan_hc$clusterID == 8, c(1:11)]

# t_test_result
hc_t_result <- data.frame()

for (i in 1:11){
  
  hc_t_result[i,1] <- t.test(hc_cluster7[,i], hc_cluster8[,i], 
                              alternative = "two.sided")$p.value
  
  hc_t_result[i,2] <- t.test(hc_cluster7[,i], hc_cluster8[,i], 
                              alternative = "greater")$p.value
  
  hc_t_result[i,3] <- t.test(hc_cluster7[,i], hc_cluster8[,i], 
                              alternative = "less")$p.value
}

hc_t_result

# Part 3: Density-based Clustering -----------------------------------------
install.packages("factoextra")
install.packages("dbscan")

library(factoextra)
library(dbscan)

data("multishapes")
df_multishapes <- multishapes[, 1:2]

set.seed(12345)

# K-Means clustering & Visulization
KMC_multishapes <- kmeans(df_multishapes, 5, nstart = 25)
fviz_cluster(KMC_multishapes, df_multishapes, ellipse = TRUE, geom = "point")

# DBSCAN & Visualization
DBSCAN_multishapes <- dbscan(df_multishapes, eps = 0.15, minPts = 5)
fviz_cluster(DBSCAN_multishapes, df_multishapes, ellipse = FALSE, geom = "point",
             show.clust.cent = FALSE)

