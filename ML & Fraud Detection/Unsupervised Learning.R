# Libraries
library(klaR) # used for Naive-Bayes. Must be called before tidyverse, otherwise it masks `select` method
library(FactoMineR)
library(factoextra)
library(mlbench)
library(RANN)
library(e1071)
library(arules)
library(arulesViz)
library(NbClust) # to find best number of clusters
library(dplyr)
library(plot3D) # to make 3D plot for PCA
library(dbscan) # for DBSCAN clustering

# Predefined draw confusion matrix function
draw_confusion_matrix <- function(cm) {
  
  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title('CONFUSION MATRIX', cex.main=2)
  
  # create the matrix 
  rect(150, 430, 240, 370, col=colorsBasketball[2])
  text(195, 435, 'Fraud', cex=1.2)
  rect(250, 430, 340, 370, col=colorsBasketball[4])
  text(295, 435, 'Not Fraud', cex=1.2)
  text(125, 370, 'Suspect Behaviour', cex=1.3, srt=90, font=2)
  text(245, 450, 'Actual Fraud', cex=1.3, font=2)
  rect(150, 305, 240, 365, col=colorsBasketball[4])
  rect(250, 305, 340, 365, col=colorsBasketball[2])
  text(140, 400, 'Fraud', cex=1.2, srt=90)
  text(140, 335, 'Not Fraud', cex=1.2, srt=90)
  
  # add in the cm results 
  res <- as.numeric(cm$table)
  text(195, 400, res[1], cex=1.6, font=2, col='white')
  text(195, 335, res[2], cex=1.6, font=2, col='white')
  text(295, 400, res[3], cex=1.6, font=2, col='white')
  text(295, 335, res[4], cex=1.6, font=2, col='white')
  
  # add in the specifics 
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
  text(10, 85, names(cm$byClass[1]), cex=1.2, font=2)
  text(10, 70, round(as.numeric(cm$byClass[1]), 3), cex=1.2)
  text(30, 85, names(cm$byClass[2]), cex=1.2, font=2)
  text(30, 70, round(as.numeric(cm$byClass[2]), 3), cex=1.2)
  text(50, 85, names(cm$byClass[5]), cex=1.2, font=2)
  text(50, 70, round(as.numeric(cm$byClass[5]), 3), cex=1.2)
  text(70, 85, names(cm$byClass[6]), cex=1.2, font=2)
  text(70, 70, round(as.numeric(cm$byClass[6]), 3), cex=1.2)
  text(90, 85, names(cm$byClass[7]), cex=1.2, font=2)
  text(90, 70, round(as.numeric(cm$byClass[7]), 3), cex=1.2)
  
  # add in the accuracy information 
  text(30, 35, names(cm$overall[1]), cex=1.5, font=2)
  text(30, 20, round(as.numeric(cm$overall[1]), 3), cex=1.4)
  text(70, 35, names(cm$overall[2]), cex=1.5, font=2)
  text(70, 20, round(as.numeric(cm$overall[2]), 3), cex=1.4)
}  



# ================= SCALE DATA =================
# Total data
scaled_data_big <- data %>%
  # select only some columns
  dplyr::select(amount, gender, age, category, merchant_total_trans, customer_total_trans, merchant_ID, category_total_trans,
                customer_ID, amount_thresh, amount_thresh_total_trans) %>% 
  scale() %>% 
  as.matrix()

# ================= KMEANS =================
set.seed(123)
kmeans_clust_3 <- kmeans(scaled_data_big, centers = 3, nstart = 25)

# Append cluster No. 
kmeans_data <- scaled_data_big %>% 
  as.data.frame() %>% 
  mutate(cluster = kmeans_clust_3$cluster)

# Check the clusters against the fraud label
x <- kmeans_data %>% 
  mutate(fraud = data$fraud)
table(x$cluster, x$fraud)



# Compute the mean of each variable by cluster
aggregate(kmeans_data, by = list(cluster = kmeans_clust_3$cluster), mean)


# ================ PCA =====================
# Principal Component object
pca_object <- prcomp(kmeans_data[,c(1:4)], center = TRUE,scale. = TRUE)

# Eigenvalues for Dimensions variability explained
eigenvalues <- get_eig(pca_object)
eigenvalues



# Keep only first 3 PCs and append cluster column
pca_data <- pca_object$x[, c(1:3)] %>% 
  as.data.frame() %>% 
  mutate(cluster = kmeans_data$cluster)

# x, y and z coordinates
x <- pca_data$PC1
y <- pca_data$PC2
z <- pca_data$PC3

# Visualise the clusters
scatter3D(x, y, z, col.var = as.integer(pca_data$cluster), col = c(colorsBasketball[1], colorsBasketball[2], colorsBasketball[4]),
          colkey = FALSE, main ="Kmeans clusters", phi = 0, bty ="g", pch = 20, cex = 2,
          xlab = "PC1", ylab = "PC2", zlab = "PC3")



# ========== Computing the distance ============
# the Euclidean distance
distances <- sqrt(rowSums(scaled_data_big - fitted(kmeans_clust_3)) ^ 2)

# Finding the outliers
outliers <- boxplot.stats(distances)$out

# Finding the index positions of the outliers
index_outliers <- which(distances %in% outliers)

# Flag the outliers in the data and create final dataset
kmeans_data_final <- data %>%
  mutate(index = row_number(),
         cluster = kmeans_clust_3$cluster,
         suspect_behaviour = ifelse(index %in% index_outliers, "F", "NF"))


# ========== Assessing K MEANS ============
# Plotting Confusion Matrix
cm_kmeans <- confusionMatrix(reference = as.factor(kmeans_data_final$fraud), data = as.factor(kmeans_data_final$suspect_behaviour))
draw_confusion_matrix(cm_kmeans)



# Filter only cluster 1
cluster_1 <- kmeans_data_final %>% 
  filter(cluster == 1)

# Inpect data
dim(cluster_1)

table(cluster_1$fraud)


# How do the 5 observations look?
cluster_1 %>% 
  filter(fraud == "F")


# Filter only cluster 3
cluster_2 <- kmeans_data_final %>% 
  filter(cluster == 2)

# Inpect data
dim(cluster_2)
table(cluster_2$fraud)


# Inspect non fraud
cluster_2 %>% 
  filter(fraud == "NF") %>% 
  group_by(category) %>% 
  summarise(n())


cluster_2 %>% 
  filter(fraud == "NF") %>% 
  summarise(mean(amount))


# Inspect fraud
cluster_2 %>% 
  filter(fraud == "F") %>% 
  group_by(category) %>% 
  summarise(n())


cluster_2 %>% 
  filter(fraud == "F") %>% 
  summarise(mean(amount))


# Filter only cluster 3
cluster_3 <- kmeans_data_final %>% 
  filter(cluster == 3)

# Inpect data
dim(cluster_3)

table(cluster_3$fraud)

# Inspect non fraud
cluster_3 %>% 
  filter(fraud == "NF") %>% 
  group_by(category) %>% 
  summarise(n())

cluster_3 %>% 
  filter(fraud == "NF") %>% 
  summarise(mean(amount))

# Inspect fraud
cluster_3 %>% 
  filter(fraud == "F") %>% 
  group_by(category) %>% 
  summarise(n())

cluster_3 %>% 
  filter(fraud == "F") %>% 
  summarise(mean(amount))




