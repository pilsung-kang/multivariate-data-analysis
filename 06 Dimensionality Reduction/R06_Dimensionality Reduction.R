# Install necessary packages
# glmnet: Ridge, Lasso, Elastic Net Logistic Regression 
# GA: genetic algorithm
install.packages("glmnet")
install.packages("GA")

library(glmnet)
library(GA)

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

Perf_Table <- matrix(0, nrow = 8, ncol = 6)
rownames(Perf_Table) <- c("All", "Forward", "Backward", "Stepwise", "GA", "Ridge", "Lasso", "Elastic Net")
colnames(Perf_Table) <- c("TPR", "Precision", "TNR", "Accuracy", "BCR", "F1-Measure")

# Load the data & Preprocessing
Ploan <- read.csv("Personal Loan.csv")

Ploan_input <- Ploan[,-c(1,5,10)]
Ploan_input_scaled <- scale(Ploan_input, center = TRUE, scale = TRUE)
Ploan_target <- Ploan$Personal.Loan

Ploan_data_scaled <- data.frame(Ploan_input_scaled, Ploan_target)

trn_idx <- 1:1500
tst_idx <- 1501:2500

Ploan_trn <- Ploan_data_scaled[trn_idx,]
Ploan_tst <- Ploan_data_scaled[tst_idx,]

# Variable selection method 0: Logistic Regression with all variables
full_model <- glm(Ploan_target ~ ., family=binomial, Ploan_trn)
summary(full_model)
full_model_coeff <- as.matrix(full_model$coefficients, 12, 1)
full_model_coeff

# Make prediction
full_model_prob <- predict(full_model, type = "response", newdata = Ploan_tst)
full_model_prey <- rep(0, nrow(Ploan_tst))
full_model_prey[which(full_model_prob >= 0.5)] <- 1
full_model_cm <- table(Ploan_tst$Ploan_target, full_model_prey)
full_model_cm

# Peformance evaluation
Perf_Table[1,] <- perf_eval(full_model_cm)
Perf_Table

# Variable selection method 1: Forward selection
tmp_x <- paste(colnames(Ploan_trn)[-12], collapse=" + ")
tmp_xy <- paste("Ploan_target ~ ", tmp_x, collapse = "")
as.formula(tmp_xy)

forward_model <- step(glm(Ploan_target ~ 1, data = Ploan_trn), 
                      scope = list(upper = as.formula(tmp_xy), lower = Ploan_target ~ 1), 
                      direction="forward", trace = 1)
summary(forward_model)
forward_model_coeff <- as.matrix(forward_model$coefficients, 12, 1)
forward_model_coeff

# Make prediction
forward_model_prob <- predict(forward_model, type = "response", newdata = Ploan_tst)
forward_model_prey <- rep(0, nrow(Ploan_tst))
forward_model_prey[which(forward_model_prob >= 0.5)] <- 1
forward_model_cm <- table(Ploan_tst$Ploan_target, forward_model_prey)
forward_model_cm

# Peformance evaluation
Perf_Table[2,] <- perf_eval(forward_model_cm)
Perf_Table

# Variable selection method 2: Backward elimination
backward_model <- step(full_model, 
                       scope = list(upper = as.formula(tmp_xy), lower = Ploan_target ~ 1),
                       direction = "backward", trace = 1)
summary(backward_model)
backward_model_coeff <- as.matrix(backward_model$coefficients, 12, 1)
backward_model_coeff

# Make prediction
backward_model_prob <- predict(backward_model, type = "response", newdata = Ploan_tst)
backward_model_prey <- rep(0, nrow(Ploan_tst))
backward_model_prey[which(backward_model_prob >= 0.5)] <- 1
backward_model_cm <- table(Ploan_tst$Ploan_target, backward_model_prey)
backward_model_cm

# Peformance evaluation
Perf_Table[3,] <- perf_eval(backward_model_cm)
Perf_Table

# Variable selection method 3: Stepwise selection
tmp_x <- paste(colnames(Ploan_trn)[-12], collapse=" + ")
tmp_xy <- paste("Ploan_target ~ ", tmp_x, collapse = "")
as.formula(tmp_xy)

stepwise_model <- step(glm(Ploan_target ~ 1, data = Ploan_trn), 
                       scope = list(upper = as.formula(tmp_xy), lower = Ploan_target ~ 1), 
                       direction="both", trace = 1)
summary(stepwise_model)
stepwise_model_coeff <- as.matrix(stepwise_model$coefficients, 12, 1)
stepwise_model_coeff

# Make prediction
stepwise_model_prob <- predict(stepwise_model, type = "response", newdata = Ploan_tst)
stepwise_model_prey <- rep(0, nrow(Ploan_tst))
stepwise_model_prey[which(stepwise_model_prob >= 0.5)] <- 1
stepwise_model_cm <- table(Ploan_tst$Ploan_target, stepwise_model_prey)
stepwise_model_cm

# Peformance evaluation
Perf_Table[4,] <- perf_eval(stepwise_model_cm)
Perf_Table

# Variable selection method 4: Genetic Algorithm
# Fitness function: F1 for the training dataset
fit_F1 <- function(string){
  sel_var_idx <- which(string == 1)
  # Use variables whose gene value is 1
  sel_x <- x[, sel_var_idx]
  xy <- data.frame(sel_x, y)
  # Training the model
  GA_lr <- glm(y ~ ., family = binomial, data = xy)
  GA_lr_prob <- predict(GA_lr, type = "response", newdata = xy)
  GA_lr_prey <- rep(0, length(y))
  GA_lr_prey[which(GA_lr_prob >= 0.5)] <- 1
  GA_lr_cm <- matrix(0, nrow = 2, ncol = 2)
  GA_lr_cm[1,1] <- length(which(y == 0 & GA_lr_prey == 0))
  GA_lr_cm[1,2] <- length(which(y == 0 & GA_lr_prey == 1))
  GA_lr_cm[2,1] <- length(which(y == 1 & GA_lr_prey == 0))
  GA_lr_cm[2,2] <- length(which(y == 1 & GA_lr_prey == 1))
  GA_perf <- perf_eval(GA_lr_cm)
  return(GA_perf[6])
}

x <- as.matrix(Ploan_trn[,-12])
y <- Ploan_trn[,12]

# Variable selection by Genetic Algorithm
start_time <- proc.time()
GA_F1 <- ga(type = "binary", fitness = fit_F1, nBits = ncol(x), 
            names = colnames(x), popSize = 50, pcrossover = 0.5, 
            pmutation = 0.01, maxiter = 100, elitism = 2, seed = 123)
end_time <- proc.time()
end_time - start_time

best_var_idx <- which(GA_F1@solution == 1)

# Model training based on the best variable subset
GA_trn_data <- Ploan_trn[,c(best_var_idx, 12)]
GA_tst_data <- Ploan_tst[,c(best_var_idx, 12)]

GA_model <- glm(Ploan_target ~ ., family=binomial, GA_trn_data)
summary(GA_model)
GA_model_coeff <- as.matrix(GA_model$coefficients, 12, 1)
GA_model_coeff

# Make prediction
GA_model_prob <- predict(GA_model, type = "response", newdata = GA_tst_data)
GA_model_prey <- rep(0, nrow(Ploan_tst))
GA_model_prey[which(GA_model_prob >= 0.5)] <- 1
GA_model_cm <- table(GA_tst_data$Ploan_target, GA_model_prey)
GA_model_cm

# Peformance evaluation
Perf_Table[5,] <- perf_eval(GA_model_cm)
Perf_Table

# Shrinkage method 1: Ridge logistic regression
Ploan_trn_X <- as.matrix(Ploan_trn[,-12])
Ploan_trn_y <- as.factor(Ploan_trn[,12])

Ploan_tst_X <- as.matrix(Ploan_tst[,-12])
Ploan_tst_y <- as.factor(Ploan_tst[,12])

Ridge_model <- glmnet(Ploan_trn_X, Ploan_trn_y, family = "binomial", alpha = 0)
plot(Ridge_model, xvar = "lambda")

# Find the best lambda based in 5-fold cross validation
CV_Ridge <- cv.glmnet(Ploan_trn_X, Ploan_trn_y, family = "binomial", alpha = 0)
plot(CV_Ridge)
best_lambda <- CV_Ridge$lambda.min

# Check the coefficients
Ridge_model_coeff <- predict(Ridge_model, s = best_lambda, newx = Ploan_tst_X, type = "coefficient")
Ridge_model_coeff

# Make predictions
Ridge_model_prey <- predict(Ridge_model, s = best_lambda, newx = Ploan_tst_X, type = "class")
Ridge_model_prey <- as.factor(Ridge_model_prey)
Ridge_model_cm <- table(Ploan_tst_y, Ridge_model_prey)
Ridge_model_cm

# Peformance evaluation
Perf_Table[6,] <- perf_eval(Ridge_model_cm)
Perf_Table

# Shrinkage method 2: Lasso regression
Lasso_model <- glmnet(Ploan_trn_X, Ploan_trn_y, family = "binomial", alpha = 1)
plot(Lasso_model, xvar = "lambda")

# Find the best lambda based in 5-fold cross validation
CV_Lasso <- cv.glmnet(Ploan_trn_X, Ploan_trn_y, family = "binomial", alpha = 1)
plot(CV_Lasso)
best_lambda <- CV_Lasso$lambda.min

# Check the coefficients
Lasso_model_coeff <- predict(Lasso_model, s = best_lambda, newx = Ploan_tst_X, type = "coefficient")
Lasso_model_coeff

# Make predictions
Lasso_model_prey <- predict(Lasso_model, s = best_lambda, newx = Ploan_tst_X, type = "class")
Lasso_model_prey <- as.factor(Lasso_model_prey)
Lasso_model_cm <- table(Ploan_tst_y, Lasso_model_prey)
Lasso_model_cm

# Peformance evaluation
Perf_Table[7,] <- perf_eval(Lasso_model_cm)
Perf_Table

# Shrinkage method 3: Elastic net regression
Elastic_model <- glmnet(Ploan_trn_X, Ploan_trn_y, family = "binomial", alpha = 0.5)
plot(Elastic_model, xvar = "lambda")

# Find the best lambda based in 5-fold cross validation
CV_Elastic <- cv.glmnet(Ploan_trn_X, Ploan_trn_y, family = "binomial", alpha = 0.5)
plot(CV_Elastic)
best_lambda <- CV_Elastic$lambda.min

# Check the coefficients
Elastic_model_coeff <- predict(Elastic_model, s = best_lambda, newx = Ploan_tst_X, type = "coefficient")
Elastic_model_coeff

# Make predictions
Elastic_model_prey <- predict(Elastic_model, s = best_lambda, newx = Ploan_tst_X, type = "class")
Elastic_model_prey <- as.factor(Elastic_model_prey)
Elastic_model_cm <- table(Ploan_tst_y, Elastic_model_prey)
Elastic_model_cm

# Peformance evaluation
Perf_Table[8,] <- perf_eval(Elastic_model_cm)
Perf_Table

