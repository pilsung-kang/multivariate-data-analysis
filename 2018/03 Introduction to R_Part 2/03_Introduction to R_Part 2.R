# Part 3: Conditions and Repititions --------------------------------------

# Condition
r <- 1
if (r==4) {
  printf("The valus of r is 4")
} else {
  print("The valus of r is not 4")
}

carbon <- c(10, 12, 15, 19, 20)
if (mean(carbon) > median(carbon)) {
  print ("Mean > Median")
} else {
  print ("Median <= Mean")
}

# Caution!
if (mean(carbon) > median(carbon)) {
  print ("Mean > Median")
} 
else {
  print ("Median <= Mean")
}

# ifelse example
x <- 1:10
y <- ifelse(x%%2 == 0, "even", "odd")
y

# loop
n <- c(5,10,15)

for (i in n) {
  print(i^2)
}

i <- 1
while (i <= 10) {
  i <- i+4
  print(i)
}

i <- 1
repeat {
  i <- i+4
  print(i)
  if (i > 10) break
}


# Part 4: Functions -------------------------------------------------------

# User written functions
mean.and.sd1 <- function(x) {
  av <- mean(x)
  sdev <- sd(x)
  return(c(mean=av, SD=sdev))
}

distance <- c(148, 182, 173, 166, 109, 141, 166)
mean.and.sd1(distance)

mean.and.sd2 <- function(x) {
  av <- mean(x)
  sdev <- sd(x)
  c(mean=av, SD=sdev)
  return(av)
}

distance <- c(148, 182, 173, 166, 109, 141, 166)
mean.and.sd2(distance)

mean.and.sd3 <- function(x = rnorm(10)) {
  av <- mean(x)
  sdev <- sd(x)
  c(mean=av, SD=sdev)
}

mean.and.sd3()
mean.and.sd3(distance)

# Function arguments
addTheLog <- function(first, second) {first + log(second)}
# Exact names
addTheLog(second=exp(4),first=1)
# Partially matching names
addTheLog(s=exp(4),first=1)
# Argument order
addTheLog(1,exp(4))

# Return the result with return()
oddcount <- function(x) {
  k <- 0
  print(sprintf("odd number calculator"))
  for (n in 1:x) {
    if (n %% 2 == 1) {
      cat(sprintf("%d is an odd number. \n", n))
      k <- k+1
    }
  }
  return(k)
}

oddcount(10)

# Return the result without return() but explicitly designate the object
oddcount <- function(x) {
  k <- 0
  print(sprintf("odd number calculator"))
  for (n in 1:x) {
    if (n %% 2 == 1) {
      cat(sprintf("%d is an odd number. \n", n))
      k <- k+1
    }
  }
  k
}

oddcount(10)

# Return the result without either return() or explicit designation
oddcount <- function(x) {
  k <- 0
  print(sprintf("odd number calculator"))
  for (n in 1:x) {
    if (n %% 2 == 1) {
      cat(sprintf("%d is an odd number. \n", n))
      k <- k+1
    }
  }
}

oddcount(10)

# Function example 1
findrepeats <- function(x, k) {
  n <- length(x)
  repeats <- NULL
  for (i in 1:(n-k+1)) {
    if(all(x[i:(i+k-1)] == 1)) repeats <- c(repeats, i)
  }
  return(repeats)
}

vec <- c(0,1,1,0,0,1,1,1,0,1,1)
findrepeats(vec,2)
findrepeats(vec,3)
findrepeats(vec,4)

# Example 2: Kendall's tau
findud <- function(v) {
  vud <- v[-1] - v[-length(v)]
  return(ifelse(vud >0, 1, -1))
}

udcorr <- function(x,y) {
  ud <- lapply(list(x,y), findud)
  return(mean(ud[[1]] == ud[[2]]))
}

temp <- c(10, 15, 13, 17, 20)
pressure <- c(900, 920, 890, 940, 920)

udcorr(temp,pressure)


# Part 5: R Graphs --------------------------------------------------------
# R에서 사용하는 그래프 
data(iris)
x <- iris[,1]
y <- iris[,2]
subiris <- iris[,1:2]

# 그래프의 다형성: 입력 인자에 따라 다른 형태의 그래프가 생성 
plot(x,y)
plot(subiris)
plot(iris)

# 제목, x-y 축 이름 달기 
plot(subiris, main="The comparison between length and width",
     xlab = "The length of sepal",
     ylab = "The width of sepal")

# 그래프 객체의 색 및 모양 변경하기 
plot(iris[,1],iris[,2],pch=as.integer(iris[,5]))

# 사용 가능한 색 및 모양 예시 
plot(iris$Sepal.Length,iris$Sepal.Width,
     pch=as.integer(iris$Species),col=as.integer(iris$Species)+10)

# 사용 가능한 색 및 모양 예시 
plot(0,0, xlim=c(0,13), ylim=c(0,4), type="n")
xpos <- rep((0:12)+0.5,2)
ypos <- rep(c(3,1), c(13,13))
points(xpos, ypos, cex=seq(from=1,to=3,length=26), col=1:26, pch=0:25)
text(xpos, ypos, labels = paste(0:25), cex=seq(from=0.1,to=1,length=26))

# 조건화 그래프 
coplot(iris[,1]~iris[,2] | iris[,5])

# 막대그래프 
data(airquality)
heights <- tapply(airquality$Temp, airquality$Month, mean)
barplot(heights)
barplot(heights, main="Mean Temp. by Month",
        names.arg = c("May", "Jun", "Jul", "Aug", "Sep"),
        ylab = "Temp (deg.F)")

# 막대그래프 꾸미기 
rel.hts <- (heights-min(heights))/(max(heights)-min(heights))
grays <- gray(1-rel.hts)
barplot(heights, col=grays, ylim=c(50,90), xpd=FALSE,
        main="Mean Temp. by Month",
        names.arg = c("May", "Jun", "Jul", "Aug", "Sep"),
        ylab = "Temp (deg.F)")

# 히스토그램 그리기 
samp <- rgamma(500,2,2)
hist(samp, 20, prob=T)
lines(density(samp))

# 그림을 파일로 저장하기: png 형식 
png("Hist_dist.png")
hist(samp, 20, prob=T)
lines(density(samp))
dev.off()

# 그림을 파일로 저장하기: pdf 형식 
pdf("Hist_dist.pdf")
hist(samp, 20, prob=T)
lines(density(samp))
dev.off()

# 보다 다양한 그래프 생성을 위해 ggplot2 패키기 이용 
install.packages("ggplot2")
library(ggplot2) 
data(mtcars)

# 그래프 도시를 위한 팩터 생성  
mtcars$gear <- factor(mtcars$gear,levels=c(3,4,5), labels=c("3gears","4gears","5gears")) 
mtcars$am <- factor(mtcars$am,levels=c(0,1), labels=c("Automatic","Manual")) 
mtcars$cyl <- factor(mtcars$cyl,levels=c(4,6,8), labels=c("4cyl","6cyl","8cyl")) 

# 연비(mpg)에 대한 커널 밀도 함수 추정
# 기어의 숫자에 따른 그룹(색상)별로 도시
qplot(mpg, data=mtcars, geom="density", fill=gear, 
      alpha=I(.5), main="Distribution of Gas Milage", 
      xlab="Miles Per Gallon", ylab="Density")

# 각 기어(gear)-실린더 조합에 따른 연비(mpg)와 마력(hp)의 산점도
# 각 산점도에서 변속기(am)은 색상과 모양으로 구분됨
qplot(hp, mpg, data=mtcars, shape=am, color=am, 
      facets=gear~cyl, size=I(3),
      xlab="Horsepower", ylab="Miles per Gallon")

# 실린더 갯수에 따라 공차중량(wt)과 연비(mpg)를 회귀선으로 표현 
p <- ggplot(mtcars, aes(y=mpg, x=wt, colour=factor(cyl))) 
p <- p + ggtitle("Regression of MPG on Weight") 
p <- p + stat_smooth(method=lm, aes(fill = factor(cyl))) + geom_point()
p

# 기어의 숫자에 따른 연비의 상자그림 
# 실제 관측치들을 점으로 표현
qplot(gear, mpg, data=mtcars, geom=c("boxplot", "jitter"), 
      fill=gear, main="Mileage by Gear Number",
      xlab="", ylab="Miles per Gallon")

