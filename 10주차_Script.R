# 두 범주형 변수의 요약
x1 <- c('A','B','C','A');x1
x1 <- as.factor(x1);x1

x2 <- c('X','Y','Y','X');x2
x2 <- as.factor(x2);x2

table(x1,x2)

# 두 연속형 변수의 요약
x1 <- c(1,2,3,4,5);x1
x2 <- c(10,9,8,7,6);x2

plot(x1,x2)

cor(x1,x2) 

## 단순선형회귀모형 
# 예제. 다음 표는 어떤 알레르기 증세에 효과가 있다고 하는 새로 개발된 약품의 복용량(mg)과 효과가 지속되는 기간(일)을 기록한 자료이다. 
x <- c(3,3,4,5,6,6,7,8,8,9);x
y <- c(9,5,12,9,14,16,22,18,24,22);y

plot(x,y) # 점들이 어떤 직선 주위로 흩어져 나타남. 
# 두 변수 사이에 근사적인 직선관계가 있음을 확인함. 
# 완벽한 직선의 관계는 아닌 점에 주의. 
# 여기서는 직선관계로 나타났지만, 직선관계가 아닌 경우 데이터를 변환하여 직선 관계를 만들어 줄 수 있음.

xbar <- mean(x);xbar
Sxx <- (x-xbar)%*%(x-xbar);Sxx

ybar <- mean(y);ybar
Sxy <- (x-xbar)%*%(y-ybar);Sxy

beta1_hat <- Sxy/Sxx;beta1_hat
beta0_hat <- ybar-beta1_hat*xbar;beta0_hat

pred <- beta0_hat + beta1_hat*x;pred
res <- y - pred;res

sum(res)

SSE <- res%*%res;SSE

n <- length(x);n
MSE <- SSE/(n-2);MSE

model <- lm(y~x);model
summary(model)

pred <- predict(model);pred
res <- y - pred;res
sum(res)
SSE <- res%*%res;SSE
MSE <- SSE/(n-2);MSE
