# Performance Evaluation Function -----------------------------------------
perf_eval <- function(cm){
  
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

# Evaluate the performance
perf_mat <- matrix(0, 4, 6)
rownames(perf_mat) <- c("LR_ALL", "LR_Forward", "LR_Backward", "LR_Stepwise")
colnames(perf_mat) <- c("TPR", "Precision", "TNR", "ACC", "BCR", "F1")
perf_mat

# Logistic Regression -----------------------------------------------------
# Conduct the normalization
ploan <- read.csv("Personal Loan.csv")

input_idx <- c(2:9,11:14)
target_idx <- 10

# Select the input variables and normalize them
ploan_input <- ploan[,input_idx]
ploan_input <- scale(ploan_input, center = TRUE, scale = TRUE)

# Select the target variable
ploan_target <- ploan[,target_idx]

# Combind the normailzed input data and the target data
ploan_data <- data.frame(ploan_input, ploan_target)

# Split the data into the training/validation sets
trn_idx <- sample(1:dim(ploan_data)[1], round(0.7*dim(ploan_data)[1]))

# Divide the entire dataset into traning and valiation
ploan_trn <- ploan_data[trn_idx,]
ploan_val <- ploan_data[-trn_idx,]

# Train the Logistic Regression Model with all variables
full_lr <- glm(ploan_target ~ ., family=binomial, ploan_trn)
full_lr
summary(full_lr)

# Train the Logistic Regression Model with forward selection
tmp_x <- paste(colnames(ploan_trn)[-13], collapse=" + ")
tmp_xy <- paste("ploan_target ~ ", tmp_x, collapse = "")
tmp_xy
as.formula(tmp_xy)

forward_lr <- step(lm(ploan_target ~ 1, data = ploan_trn), 
                   scope = list(upper = as.formula(tmp_xy), lower = Price ~ 1),
                   direction="forward", trace=1)
summary(forward_lr)

# Train the Logistic Regression Model with backward elimination
backward_lr <- step(full_lr, scope = list(upper = as.formula(tmp_xy), lower = ploan_target ~ 1),
                    direction="backward", trace=1)
summary(backward_lr)

# Train the Logistic Regression Model with stepwise selection
stepwise_lr <- step(lm(ploan_target ~ 1, data = ploan_trn), 
                   scope = list(upper = as.formula(tmp_xy), lower = Price ~ 1),
                   direction="both", trace=1)
summary(stepwise_lr)

# Evaluate the logistic regression performance on the validation data
# Case 1: full model
full_response <- predict(full_lr, type = "response", newdata = ploan_val)
full_target <- ploan_val$ploan_target
full_predicted <- rep(0, length(full_target))
full_predicted[which(full_response >= 0.5)] <- 1
cm_full <- table(full_target, full_predicted)
perf_mat[1,] <- perf_eval(cm_full)

# Case 2: forward model
forward_response <- predict(forward_lr, type = "response", newdata = ploan_val)
forward_target <- ploan_val$ploan_target
forward_predicted <- rep(0, length(forward_target))
forward_predicted[which(forward_response >= 0.5)] <- 1
cm_forward <- table(forward_target, forward_predicted)
perf_mat[2,] <- perf_eval(cm_forward)

# Case 3: backward
backward_response <- predict(backward_lr, type = "response", newdata = ploan_val)
backward_target <- ploan_val$ploan_target
backward_predicted <- rep(0, length(backward_target))
backward_predicted[which(backward_response >= 0.5)] <- 1
cm_backward <- table(backward_target, backward_predicted)
perf_mat[3,] <- perf_eval(cm_backward)

# Case 4: stepwise
stepwise_response <- predict(stepwise_lr, type = "response", newdata = ploan_val)
stepwise_target <- ploan_val$ploan_target
stepwise_predicted <- rep(0, length(stepwise_target))
stepwise_predicted[which(stepwise_response >= 0.5)] <- 1
cm_stepwise <- table(stepwise_target, stepwise_predicted)
perf_mat[4,] <- perf_eval(cm_stepwise)

# Compare the confusion matrices
cm_full
cm_forward
cm_backward
cm_stepwise

# Compare the classification performances
perf_mat
