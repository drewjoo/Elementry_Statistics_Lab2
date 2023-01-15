## 선형 관계의 정도 
#예제. 다음 표는 어떤 알레르기 증세에 효과가 있다고 하는 새로 개발된 약품의 복용량(mg)과 효과가 지속되는 기간(일)을 기록한 자료이다.
x <- c(3,3,4,5,6,6,7,8,8,9);x
y <- c(9,5,12,9,14,16,22,18,24,22);y

plot(x,y)

xbar <- mean(x);xbar
Sxx <- (x-xbar)%*%(x-xbar);Sxx

ybar <- mean(y);ybar
Sxy <- (x-xbar)%*%(y-ybar);Sxy
 
Syy <- (y-ybar)%*%(y-ybar);Syy

SST <- Syy;SST
SSE <- Syy - Sxy^2/Sxx;SSE
SSR <- Sxy^2/Sxx;SSR

SST-SSR;SSE

beta1_hat <- Sxy/Sxx;beta1_hat
beta0_hat <- ybar-beta1_hat*xbar;beta0_hat

pred <- beta0_hat + beta1_hat*x;pred
res <- y - pred;res

res%*%res;SSE

R2 <- SSR/SST;R2
1-SSE/SST
cor(x,y)^2


## lm 함수
model <- lm(y~x);model
summary(model)

res <- residuals(model);res # 잔차
res%*%res;SSE
(2.821)^2*8

beta <- coef(model);beta # 회귀계수

confint(model) # 회귀계수의 95% 신뢰구간
confint(model, level=0.90) # 회귀계수의 90% 신뢰구간

fitted(model) # 적합값 - 기존 데이터
predict(model) # 적합값
predict(model, newdata=data.frame(x=7.5)) # 새로운 값에 대한 예측값

predict(model, newdata=data.frame(x=seq(min(x),max(x))))
predict(model, newdata=data.frame(x=seq(min(x),max(x))),interval='none')
conf_int <- predict(model, newdata=data.frame(x=seq(min(x),max(x))),interval='confidence');conf_int # 평균반응에 대한 신뢰구간
pred_int <- predict(model, newdata=data.frame(x=seq(min(x),max(x))),interval='prediction');pred_int # 반응변수값에 대한 신뢰구간: 예측구간

plot(x,y)
abline(coef(model),col='red')
matplot(seq(min(x),max(x)), conf_int, type = "n")
matlines(seq(min(x),max(x)), conf_int,
         lty = c(1, 2, 2),col=c('red','blue','blue'))
matlines(seq(min(x),max(x)), pred_int,
         lty = c(1, 2, 2),col=c('red','green','green'))

deviance(model) # 잔차제곱합

## 잔차진단
# 잔차 plot - 잔차 vs. 적합값 & 정규확률그림
par(mfrow=c(2,2))
plot(model)
plot(model,which=c(1:3,5))

par(mfrow=c(3,2))
plot(model,which=c(1:6))

# 잔차 vs. 독립변수값
par(mfrow=c(1,1))
plot(x,res)
abline(h=0,lty=2)

# 시간에 따른 잔차 그림
index <- 1:length(res);index
plot(index,res, xlab='관측시간')
abline(h=0,lty=2)

# e_i vs. e_(i-1)
plot(res[1:length(res)-1],res[2:length(res)],
     xlab='e_(i-1)',ylab='e_i')

# 부호가 반대면 잔차 vs. 독립변수값 그림과 잔차 vs. 적합값 그림 경향 반대
y_neg <- -y;y_neg
x
m <- lm(y_neg~x);m
plot(x,residuals(m))
plot(fitted(m),residuals(m))
