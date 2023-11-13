# Libraries
library(ggplot2) # plot library
library(tidyverse) # for data manipulation
library(gridExtra) # multiple plots in 1
library(ggrepel) # for graph repel (labels)
library(scales) # for % in density plots


# Predefined personal color schemes
colorsBasketball <- c("#F57E00", "#FFA90A", "#FFCE72", "#3AAFF9", "#0087DC", "#005991")
colors60s <- c("#BF4402", "#94058E", "#005DD7", "#2690C3", "#F5C402", "#CE378E")

# Predefined theme
my_theme <- theme(plot.background = element_rect(fill = "grey97", color = "grey25"),
                  panel.background = element_rect(fill = "grey97"),
                  panel.grid.major = element_line(colour = "grey87"),
                  text = element_text(color = "grey25"),
                  plot.title = element_text(size = 18),
                  plot.subtitle = element_text(size = 14),
                  axis.title = element_text(size = 11),
                  legend.box.background = element_rect(color = "grey25", fill = "grey97", size = 0.5),
                  legend.box.margin = margin(t = 5, r = 5, b = 5, l = 5))

# Read in the data
data <- read_csv()


# Preprocessing steps
data <- data %>% 
  # remove columns with 1 constant value
  dplyr::select(-zipcodeOri, -zipMerchant) %>% 
  
  # remove comas
  mutate(customer = gsub("^.|.$", "", customer),
         age = gsub("^.|.$", "", age),
         gender = gsub("^.|.$", "", gender),
         merchant = gsub("^.|.$", "", merchant),
         category = gsub("^.|.$", "", category)) %>% 
  
  # remove es_ from "category"
  mutate(category = sub("es_", "", category)) %>% 
  
  # remove Unknown from Gender
  filter(gender != "U")


# Replace U in Age with "7"
data$age[which(data$age == "U")]<-"7"

# Create Amount Thresholds
data <- data %>% 
  mutate(amount_thresh = ifelse(amount<= 500, "0-500",
                                ifelse(amount<= 1000, "500-1000",
                                       ifelse(amount<= 1500, "1000-1500",
                                              ifelse(amount<= 2000, "1500-2000",
                                                     ifelse(amount<= 2500, "2000-2500",
                                                            ifelse(amount<= 3000, "2500-3000", ">3000")))))))


# Libraries
library(fastDummies) # to create dummy variables
library(caret) # for models
install.packages("caretEnsemble")
library(caretEnsemble) # to create ensembles
library(FactoMineR)
library(factoextra)
library(RANN)
library(NbClust)
library(doParallel) # enables parallel training
library(ROSE) # for oversampling
library(imbalance) # also for oversampling
library(smotefamily) # also for oversampling


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
  text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
  text(245, 450, 'Actual', cex=1.3, font=2)
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



# ======== Create ID and Total Trans for Customer and Merchant ========
customer <- data %>% 
  group_by(customer) %>% 
  summarise(customer_total_trans = n()) %>% 
  mutate(customer_ID = seq(1, 4109, 1))

merchant <- data %>% 
  group_by(merchant) %>% 
  summarise(merchant_total_trans = n()) %>% 
  mutate(merchant_ID = seq(1, 50, 1))

category <- data %>% 
  group_by(category) %>% 
  summarise(category_total_trans = n()) %>% 
  mutate(category_ID = seq(1, 15, 1))

amount_thresh <- data %>% 
  group_by(amount_thresh) %>% 
  summarise(amount_thresh_total_trans = n()) %>% 
  mutate(amount_thresh_ID = seq(1, 7, 1))


# -------------------------
data <- data %>% 
  # add the 4 new variables
  inner_join(customer, by = "customer") %>% 
  inner_join(merchant, by = "merchant") %>% 
  select(-customer, - merchant) %>% 
  
  
  # age type from chr to dbl
  mutate(age = as.double(age)) %>% 
  # gender coding and type change from chr to dbl
  mutate(gender = ifelse(gender == "M", 1,
                         ifelse(gender == "F", 2, 3))) %>% 
  
  # recode category and add total_category_trans column
  inner_join(category, by = "category") %>% 
  mutate(category = category_ID) %>% 
  select(- category_ID) %>% 
  
  # recode amount_thresh
  inner_join(amount_thresh, by = "amount_thresh") %>% 
  mutate(amount_thresh = amount_thresh_ID) %>% 
  select(-amount_thresh_ID)


# Add also for age and gender total_trans
age <- data %>% 
  group_by(age) %>% 
  summarise(age_total_trans = n())

gender <- data %>% 
  group_by(gender) %>% 
  summarise(gender_total_trans = n())

# ------------------------
data <- data %>%
  inner_join(age, by = "age") %>% 
  inner_join(gender, by = "gender")



# ======== Transform the total_trans numbers into weights ========
# This is done so the numbers will be smaller (except customer_total_trans)
total_freq = 591746

data <- data %>% 
  mutate(merchant_total_trans = round((merchant_total_trans/total_freq)*100, 5),
         category_total_trans = round((category_total_trans/total_freq)*100, 5),
         amount_thresh_total_trans = round((amount_thresh_total_trans/total_freq)*100, 5),
         age_total_trans = round((age_total_trans/total_freq)*100, 5),
         gender_total_trans = round((gender_total_trans/total_freq)*100, 5))



# ======== Remove Step ========
data <- data %>% 
  select(-step) %>% 
  select(fraud, everything())


# ======== Create Dummy Varables for Gender, Age, Category and Amount_Thresh ========
data <- dummy_cols(data, select_columns = c("age", "gender", "category", "amount_thresh"))


# ======== Recode Fraud column ========
data <- data %>% 
  mutate(fraud = ifelse(fraud == "1", "F", "NF"))

dim(data)



# Principal Component object (using )
pca_object <- prcomp(data[,c(2:6)], center = TRUE,scale. = TRUE)

# Eigenvalues for Dimensions variability explained
eigenvalues <- get_eig(pca_object)
eigenvalues


# Keep only first 2 PCs and append target column
pca_data <- pca_object$x[, c(1:2)] %>% 
  as.data.frame() %>% 
  mutate(fraud = data$fraud)

# Visualise Fraud
pca_data %>% 
  ggplot(aes(x = PC1, y = PC2)) +
  geom_point(aes(color = fraud, shape = fraud)) +
  my_theme +
  scale_color_manual(values = c(colors60s[1], colors60s[4])) +
  labs(x = "PC1", y = "PC2", title = "Fraud Spread over the first 2 Dimensionalities", subtitle = "frauds & non-frauds have clear different behaviours",
       color = "Fraud", shape = "Fraud")


set.seed(123)

# ===================================== SPLIT DATA 65% - 35% =====================================
# Because the data is very unbalanced (fraud transactions are only 1.4% from total transactions)

# Creating the fraud dataframe
fraud_data <- data %>% filter(fraud == "F")
dim(fraud_data)


# Creating the non-fraud dataframe
non_fraud_data <- data %>% filter(fraud == "NF")
dim(non_fraud_data)


# Create data Partition
index <- createDataPartition(c(non_fraud_data$age, non_fraud_data$gender, non_fraud_data$category, non_fraud_data$amount_thresh,
                               non_fraud_data$merchant_ID), p = 0.9834, list = F)

good_data <- non_fraud_data[-index, ]
dim(good_data)


# rewrite the non fraud table
non_fraud_data <- good_data

# full data 
undersampling_data <- bind_rows(non_fraud_data, fraud_data)

# Randomize - because data is chronological
set.seed(123)
undersampling_data <- undersampling_data[sample(1:nrow(undersampling_data)), ]

dim(undersampling_data)


# ============= TESTING DATA ===============
# Final Predictions
pred_xgbTree <- predict.train(model_list$xgbTree, newdata = X_test)
pred_fda <- predict.train(model_list$fda, newdata = X_test)

# Check Sens
preds_sens <- data.frame(xgbTree = sensitivity(as.factor(pred_xgbTree), as.factor(y_test)),
                         fda = sensitivity(as.factor(pred_fda), as.factor(y_test)))
print(preds_sens)
##     xgbTree       fda
## 1 0.9827778 0.9972222
# Plot confusion Matrix
cm <- confusionMatrix(as.factor(pred_xgbTree), as.factor(y_test))
draw_confusion_matrix(cm)



# Create feature importance
var_imp <- varImp(model_list$xgbTree)
var_imp$importance %>% head(10) %>% 
  rownames_to_column("Feature") %>% 
  
  ggplot(aes(x = reorder(Feature, Overall), y = Overall)) +
  geom_bar(stat = "identity", aes(fill = Overall)) +
  coord_flip() +
  geom_label(aes(label = round(Overall, 0)), size = 3) +
  scale_fill_gradient(low = colors60s[4], high = colors60s[2], guide = "none") +
  my_theme +
  labs(x = "Feature", y = "Importance", title = "Most Important Features in Fraud Classification", subtitle = "top 10 in order")



