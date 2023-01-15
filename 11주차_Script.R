##기울기 beta1에 대한 추론
#예제. 다음 표는 어떤 알레르기 증세에 효과가 있다고 하는 새로 개발된 약품의 복용량(mg)과 효과가 지속되는 기간(일)을 기록한 자료이다.
x <- c(3,3,4,5,6,6,7,8,8,9);x
y <- c(9,5,12,9,14,16,22,18,24,22);y

plot(x,y)

xbar <- mean(x);xbar
Sxx <- (x-xbar)%*%(x-xbar);Sxx

ybar <- mean(y);ybar
Sxy <- (x-xbar)%*%(y-ybar);Sxy

# 점추정
beta1_hat <- Sxy/Sxx;beta1_hat
beta0_hat <- ybar-beta1_hat*xbar;beta0_hat

# 95% 신뢰구간
pred <- beta0_hat + beta1_hat*x;pred
res <- y - pred;res

SSE <- res%*%res;SSE

n <- length(x);n
MSE <- SSE/(n-2);MSE

s <- sqrt(MSE);s

se_hat <- s/sqrt(Sxx)
t_0.025 <- qt(0.025,n-2,lower.tail=F);t_0.025

c(beta1_hat-t_0.025*se_hat,beta1_hat+t_0.025*se_hat)

# 검정
# 약의 복용량이 증가함에 따라 효과의 지속기간도 증가하는지 유의수준 5%로 검정하라.
# H0: beta1 = 0 vs. H1: beta1 > 0
beta_10 <- 0;beta_10

t <- (beta1_hat-beta_10)/se_hat;t

t_0.05 <- qt(0.05,n-2,lower.tail=F);t_0.05

t >= t_0.05 # 기각역에 포함되므로 유의수준 5%로 귀무가설을 기각할 수 있다. 즉, 자료에서 나타난 복용량의 범위 내에서 약의 복용량이 증가하면 약의 효과의 지속기간도 증가한다는 뚜렷한 증거가 있다고 할 수 있다.

# 주의
# 귀무가설 H0: beta1 = 0 의 검정결과를 해석할 때, 귀무가설이 기각되지 않는 경우 종속변수 y가 독립변수 x에 의해 설명될 수 없다고 잘못된 결론을 내리면 안된다. 두 변수 사이에 관계가 없다기보다는 직선관계가 없다고 해석해야 한다.
# ex1. 두 변수 사이에 직선관계가 존재하지만 자료에서 얻어진 x 값의 범위 내에서는 직선관계가 나타나지 않는 경우  
# ex2. 만약 두 변수 사이에 곡선 관계가 존재한다면, 위의 검정은 부적절한 모형을 바탕으로 검정한 것

## 절편 beta0에 대한 추론
# 점추정
beta0_hat <- ybar-beta1_hat*xbar;beta0_hat

# 95% 신뢰구간
se_hat <- s*sqrt(1/n + (xbar^2)/Sxx)
t_0.025 <- qt(0.025,n-2,lower.tail=F);t_0.025

c(beta0_hat-t_0.025*se_hat,beta0_hat+t_0.025*se_hat)

# 검정
# beta0가 0인지 아닌지 유의수준 alpha=0.05에서 검정하라.
# H0: beta0 = 0 vs. H1: beta0 != 0
beta_00 <- 0;beta_00

t <- (beta0_hat-beta_00)/se_hat;t

t_0.025 <- qt(0.025,n-2,lower.tail=F);t_0.025

abs(t) >= t_0.025 # 기각역에 포함되지 않으므로, 귀무가설을 기각할 수 없다. 굳이 검정을 하지 않더라도, 바로 위에서 구한 신뢰구간을 통해서도 신뢰구간이 0을 포함하고 있기 때문에 귀무가설을 기각할 수 없음을 알 수 있다.

# 주의
# beta0 는 x = 0 에서의 종속변수의 기댓값에 대응한다. 하지만 약의 효과를 평가하는 문제에, 우선 자료에 나타난 x 값의 범위가 3과 7 사이이므로 직선식을 x = 0 으로 확장하는 것은 적절하지 못하다. 따라서 추정값 beta0_hat = -1.07이 효과지속 기간의 의미로 해석될 수 없다.

## 평균반응 beta0 + beta1 * x*에 대한 추론
x1 <- 6;x1
x2 <- 9.5;x2

# 점추정
est1 <- beta0_hat + beta1_hat * x1;est1
est2 <- beta0_hat + beta1_hat * x2;est2

# 95% 신뢰구간
se_hat1 <- s*sqrt(1/n + (x1-xbar)^2/Sxx);se_hat1
se_hat2 <- s*sqrt(1/n + (x2-xbar)^2/Sxx);se_hat2

t_0.025 <- qt(0.025,n-2,lower.tail=F);t_0.025

c(est1-t_0.025*se_hat1,est1+t_0.025*se_hat1)
c(est2-t_0.025*se_hat2,est2+t_0.025*se_hat2)

plot(x,y, xlim=c(0,10), ylim=c(0,25))
abline(a=beta0_hat,b=beta1_hat,col='red')

any_x <- seq(0,10,by=0.2);any_x

est <- beta0_hat + beta1_hat * any_x;est
se_hat <- s*sqrt(1/n + (any_x-xbar)^2/Sxx);se_hat
lower <- est-t_0.025*se_hat;lower
upper <- est+t_0.025*se_hat;upper

lines(any_x,upper,col='blue',lty=2)
lines(any_x,lower,col='blue',lty=2) # 평균 반응의 분산을 보게 되면 xbar에서 멀어질수록 분산이 커지기 때문에, 평균에서 멀어질 수록 구간이 넓어짐을 확인할 수 있다. 즉, 독립변수의 자료 범위를 크게 벗어나는 x 값에 대하여 평균반응이나 다음으로 배울 반응변수값에 대한 추정은 매우 주의해야 한다. 정확도가 떨어진다는 것 이 외에도 실제 관계가 직선이 아니고, 주어진 데이터에서만 직선이 관계일 경우처럼 추정하는 모형 자체도 문제가 있을 수 있습니다. 만약 자료의 범위를 크게 벗어나는 독립변수의 값에 대하여 추정을 원한다면 더 많은 자료를 얻어 추정하는 독립변수의 값을 포함하거나 그 값에 가깝도록 자료의 범위를 넓혀주는 등의 작업이 필요하게 된다.

## 반응변수값 beta0 + beta1 * x* + epsilon에 대한 추론
# 95% 신뢰구간: 예측구간
se_hat <- s*sqrt(1 + 1/n + (any_x-xbar)^2/Sxx);se_hat
lower <- est-t_0.025*se_hat;lower
upper <- est+t_0.025*se_hat;upper

lines(any_x,upper,col='green',lty=2)
lines(any_x,lower,col='green',lty=2) # 오차 부분이 추가가 되었기 때문에 표준오차가 조금 더 큰 값을 갖게 되어 구간이 더 넓은 것을 볼 수 있다. 각각의 하나의 값들이기 때문에 더 큰 불확실성을 가지고 있다고 볼 수 있다.

