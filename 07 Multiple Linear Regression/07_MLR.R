install.packages("moments")
library(moments)

# Performance evaluation function for regression --------------------------
perf_eval_reg <- function(tgt_y, pre_y){
  
  # RMSE
  rmse <- sqrt(mean((tgt_y - pre_y)^2))
  # MAE
  mae <- mean(abs(tgt_y - pre_y))
  # MAPE
  mape <- 100*mean(abs((tgt_y - pre_y)/tgt_y))
  
  return(c(rmse, mae, mape))
  
}

perf_mat <- matrix(0, nrow = 2, ncol = 3)

# Initialize a performance summary
rownames(perf_mat) <- c("Toyota Corolla", "Boston Housing")
colnames(perf_mat) <- c("RMSE", "MAE", "MAPE")
perf_mat

# Dataset 1: Toyota Corolla
corolla <- read.csv("ToyotaCorolla.csv")

# Indices for the activated input variables
nCar <- nrow(corolla)
nVar <- ncol(corolla)

id_idx <- c(1,2)
category_idx <- 8

# Transform a categorical variable into a set of binary variables
dummy_p <- rep(0,nCar)
dummy_d <- rep(0,nCar)
dummy_c <- rep(0,nCar)

p_idx <- which(corolla$Fuel_Type == "Petrol")
d_idx <- which(corolla$Fuel_Type == "Diesel")
c_idx <- which(corolla$Fuel_Type == "CNG")

dummy_p[p_idx] <- 1
dummy_d[d_idx] <- 1
dummy_c[c_idx] <- 1

Fuel <- data.frame(dummy_p, dummy_d, dummy_c)
names(Fuel) <- c("Petrol","Diesel","CNG")

# Prepare the data for MLR
corolla_mlr_data <- cbind(corolla[,-c(id_idx, category_idx)], Fuel)

# Split the data into the training/validation sets
set.seed(12345) 
corolla_trn_idx <- sample(1:nCar, round(0.7*nCar))
corolla_trn_data <- corolla_mlr_data[corolla_trn_idx,]
corolla_val_data <- corolla_mlr_data[-corolla_trn_idx,]

# Train the MLR
mlr_corolla <- lm(Price ~ ., data = corolla_trn_data)
mlr_corolla
summary(mlr_corolla)
plot(mlr_corolla)

# Plot the result
plot(corolla_trn_data$Price, fitted(mlr_corolla), 
     xlim = c(4000,35000), ylim = c(4000,35000))
abline(0,1,lty=3)

# normality test of residuals
corolla_resid <- resid(mlr_corolla)

m <- mean(corolla_resid)
std <- sqrt(var(corolla_resid))

hist(corolla_resid, density=20, breaks=50, prob=TRUE, 
     xlab="x-variable", main="normal curve over histogram")

curve(dnorm(x, mean=m, sd=std), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")

skewness(corolla_resid)
kurtosis(corolla_resid)

# Performance Measure
mlr_corolla_haty <- predict(mlr_corolla, newdata = corolla_val_data)

perf_mat[1,] <- perf_eval_reg(corolla_val_data$Price, mlr_corolla_haty)
perf_mat

# Dataset 2: Boston Housing
boston_housing <- read.csv("BostonHousing.csv")

nHome <- nrow(boston_housing)
nVar <- ncol(boston_housing)

# Split the data into the training/validation sets
boston_trn_idx <- sample(1:nHome, round(0.7*nHome))
boston_trn_data <- boston_housing[boston_trn_idx,]
boston_val_data <- boston_housing[-boston_trn_idx,]

# Train the MLR
mlr_boston <- lm(MEDV ~ ., data = boston_trn_data)
mlr_boston
summary(mlr_boston)
plot(mlr_boston)

# Plot the result
plot(boston_trn_data$MEDV, fitted(mlr_boston), 
     xlim = c(-5,50), ylim = c(-5,50))
abline(0,1,lty=3)

plot(fitted(mlr_boston), resid(mlr_boston), 
     xlab="Fitted values", ylab="Residuals")

# normality test of residuals
boston_resid <- resid(mlr_boston)

m <- mean(boston_resid)
std <- sqrt(var(boston_resid))

hist(boston_resid, density=20, breaks=50, prob=TRUE, 
     xlab="x-variable", main="normal curve over histogram")

curve(dnorm(x, mean=m, sd=std), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")

skewness(boston_resid)
kurtosis(boston_resid)

# Performance Measure
mlr_boston_haty <- predict(mlr_boston, newdata = boston_val_data)

perf_mat[2,] <- perf_eval_reg(boston_val_data$MEDV, mlr_boston_haty)
perf_mat
