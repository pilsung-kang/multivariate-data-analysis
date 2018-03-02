# Working directory 지정
setwd("C:\\RStudy")

# 실습 1: 전진선택/후진소거/단계적선택 ------------------------------------
# 분석에 필요한 패키지 설치 및 불러오기
# Multivariate linear regression
corolla <- read.csv("ToyotaCorolla.csv")

# Indices for the activated input variables
nCar <- dim(corolla)[1]
nVar <- dim(corolla)[2]

id_idx <- c(1,2)
category_idx <- 8

# 범주형 변수를 이진형 변수로 변환
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
mlr_data <- cbind(corolla[,-c(id_idx, category_idx)], Fuel)

# Split the data into the training/validation sets
trn_idx <- sample(1:nCar, round(0.7*nCar))
trn_data <- mlr_data[trn_idx,]
val_data <- mlr_data[-trn_idx,]

# Train the MLR
full_model <- lm(Price ~ ., data = trn_data)
full_model
summary(full_model)
plot(full_model)

# Plot the result
plot(trn_data$Price, fitted(full_model), xlim = c(4000,35000), ylim = c(4000,35000))
abline(0,1,lty=3)

anova(full_model)
plot(fitted(full_model), resid(full_model), xlab="Fitted values", ylab="Residuals")

# 변수선택 1: 전진선택법
# Upperbound formula 만들기
tmp_x <- paste(colnames(trn_data)[-1], collapse=" + ")
tmp_xy <- paste("Price ~ ", tmp_x, collapse = "")
tmp_xy
as.formula(tmp_xy)

forward_model <- step(lm(Price ~ 1, data = trn_data), 
                      scope = list(upper = as.formula(tmp_xy), lower = Price ~ 1), direction="forward", trace=1)
summary(forward_model)
anova(forward_model)

# 각 단계에서 선택된 변수 표시
forward_model$anova$Step
forward_model$anova$AIC

# 선택된 변수에 따른 AIC 감소분 표시
plot(forward_model$anova$AIC, pch = 17, cex=2, main = "AIC Decrease (Forward Selection)", xlab = "Number of Steps", ylab = "AIC")
text(forward_model$anova$AIC, forward_model$anova$Step, cex=1, pos=3, col="blue")

# 변수선택 2: 후진소거법
backward_model <- step(full_model, scope = list(upper = as.formula(tmp_xy), lower = Price ~ 1), direction="backward", trace=1)
summary(backward_model)
anova(backward_model)

# 각 단계에서 제거된 변수 표시
backward_model$anova$Step

# 제거된 변수에 따른 AIC 감소분 표시
plot(backward_model$anova$AIC, pch = 15, cex=2, main = "AIC Decrease (Backward Selection)", xlab = "Number of Steps", ylab = "AIC")
text(backward_model$anova$AIC, backward_model$anova$Step, cex=1, pos=3, col="red")

# 변수선택 3: 단계적 선택법
stepwise_model <- step(lm(Price ~ 1, data = trn_data), 
                       scope = list(upper = as.formula(tmp_xy), lower = Price ~ 1), direction="both", trace=1)
summary(stepwise_model)
anova(stepwise_model)

# 각 단계에서 선택/제거된 변수 표시
stepwise_model$anova$Step
stepwise_model$anova$AIC

# 제거/선택된 변수에 따른 AIC 감소분 표시
plot(stepwise_model$anova$AIC, pch = 19, cex=2, main = "AIC Decrease (Stepwise Selection)", xlab = "Number of Steps", ylab = "AIC")
text(stepwise_model$anova$AIC, stepwise_model$anova$Step, cex=1, pos=3, col="black")

# 검증 데이터에 대한 각 변수선택 결과의 예측 정확도 비교
full_haty <- predict(full_model, newdata = val_data)
forward_haty <- predict(forward_model, newdata = val_data)
backward_haty <- predict(backward_model, newdata = val_data)
stepwise_haty <- predict(stepwise_model, newdata = val_data)

# 회귀분석 예측성능 평가지표
# 1: Mean squared error (MSE)
perf_mat <- matrix(0,4,4)
perf_mat[1,1] <- mean((val_data$Price-full_haty)^2)
perf_mat[1,2] <- mean((val_data$Price-forward_haty)^2)
perf_mat[1,3] <- mean((val_data$Price-backward_haty)^2)
perf_mat[1,4] <- mean((val_data$Price-stepwise_haty)^2)

# 2: Root mean squared error (RMSE)
perf_mat[2,1] <- sqrt(mean((val_data$Price-full_haty)^2))
perf_mat[2,2] <- sqrt(mean((val_data$Price-forward_haty)^2))
perf_mat[2,3] <- sqrt(mean((val_data$Price-backward_haty)^2))
perf_mat[2,4] <- sqrt(mean((val_data$Price-stepwise_haty)^2))

# 3: Mean absolute error (MAE)
perf_mat[3,1] <- mean(abs(val_data$Price-full_haty))
perf_mat[3,2] <- mean(abs(val_data$Price-forward_haty))
perf_mat[3,3] <- mean(abs(val_data$Price-backward_haty))
perf_mat[3,4] <- mean(abs(val_data$Price-stepwise_haty))

# 4: Mean absolute percentage error (MAPE)
perf_mat[4,1] <- mean(abs((val_data$Price-full_haty)/val_data$Price))*100
perf_mat[4,2] <- mean(abs((val_data$Price-forward_haty)/val_data$Price))*100
perf_mat[4,3] <- mean(abs((val_data$Price-backward_haty)/val_data$Price))*100
perf_mat[4,4] <- mean(abs((val_data$Price-stepwise_haty)/val_data$Price))*100

# 변수선택 기법 결과 비교
rownames(perf_mat) <- c("MSE", "RMSE", "MAE", "MAPE")
colnames(perf_mat) <- c("All", "Forward", "Backward", "Stepwise")
perf_mat
