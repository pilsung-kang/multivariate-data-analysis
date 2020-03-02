# Performance Evaluation Function -----------------------------------------
perf_eval2 <- function(cm){
  
  # True positive rate: TPR (Recall)
  TPR <- cm[2,2]/sum(cm[2,])
  # Precision
  PRE <- cm[2,2]/sum(cm[,2])
  # True negative rate: TNR
  TNR <- cm[1,1]/sum(cm[1,])
  # Simple Accuracy
  ACC <- (cm[1,1]+cm[2,2])/sum(cm)
  # Balanced Correction Rate
  BCR <- sqrt(TPR*TNR)
  # F1-Measure
  F1 <- 2*TPR*PRE/(TPR+PRE)
  
  return(c(TPR, PRE, TNR, ACC, BCR, F1))
}

# Initialize the performance matrix
perf_mat <- matrix(0, 1, 6)
colnames(perf_mat) <- c("TPR (Recall)", "Precision", "TNR", "ACC", "BCR", "F1")
rownames(perf_mat) <- "Logstic Regression"

# Load dataset
ploan <- read.csv("Personal Loan.csv")

input_idx <- c(2,3,4,6,7,8,9,11,12,13,14)
target_idx <- 10

# Conduct the normalization
ploan_input <- ploan[,input_idx]
ploan_input <- scale(ploan_input, center = TRUE, scale = TRUE)
ploan_target <- ploan[,target_idx]
ploan_data <- data.frame(ploan_input, ploan_target)

# Split the data into the training/validation sets
set.seed(12345)
trn_idx <- sample(1:nrow(ploan_data), round(0.7*nrow(ploan_data)))
ploan_trn <- ploan_data[trn_idx,]
ploan_tst <- ploan_data[-trn_idx,]

# Train the Logistic Regression Model with all variables
full_lr <- glm(ploan_target ~ ., family=binomial, ploan_trn)
summary(full_lr)

lr_response <- predict(full_lr, type = "response", newdata = ploan_tst)
lr_target <- ploan_tst$ploan_target
lr_predicted <- rep(0, length(lr_target))
lr_predicted[which(lr_response >= 0.5)] <- 1
cm_full <- table(lr_target, lr_predicted)
cm_full

perf_mat[1,] <- perf_eval2(cm_full)
perf_mat

# Multinomial logistic regression
install.packages("nnet")
library(nnet)

perf_eval3 <- function(cm){
  
  # Simple accuracy
  ACC <- sum(diag(cm))/sum(cm)
  
  # ACC for each class
  A1 <- cm[1,1]/sum(cm[1,])
  A2 <- cm[2,2]/sum(cm[2,])
  A3 <- cm[3,3]/sum(cm[3,])
  BCR <- (A1*A2*A3)^(1/3)
  
  return(c(ACC, BCR))
}

wine <- read.csv("wine.csv")

# Define the baseline class
wine$Class <- as.factor(wine$Class)
wine$Class <- relevel(wine$Class, ref = "3") 

trn_idx <- sample(1:nrow(wine), round(0.7*nrow(wine)))
wine_trn <- wine[trn_idx,]
wine_tst <- wine[-trn_idx,]

# Train multinomial logistic regression
ml_logit <- multinom(Class ~ ., data = wine_trn)

# Check the coefficients
summary(ml_logit)
t(summary(ml_logit)$coefficients)

# Conduct 2-tailed z-test to compute the p-values
z_stats <- summary(ml_logit)$coefficients/summary(ml_logit)$standard.errors
t(z_stats)

p_value <- (1-pnorm(abs(z_stats), 0, 1))*2
options(scipen=10)
t(p_value)

cbind(t(summary(ml_logit)$coefficients), t(p_value))

# Predict the class probability
ml_logit_haty <- predict(ml_logit, type="probs", newdata = wine_tst)
ml_logit_haty[1:10,]

# Predict the class label
ml_logit_prey <- predict(ml_logit, newdata = wine_tst)

cfmatrix <- table(wine_tst$Class, ml_logit_prey)
cfmatrix
perf_eval3(cfmatrix)
