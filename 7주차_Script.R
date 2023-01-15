### 짝비교
## 신뢰구간
# 예제. 혈압약의 효과가 있는지 알아보기 위해서 15명 환자의 약 복용 전후 이완기 혈압을 측정하였다.
# 신뢰구간
# 평균 혈압강하량에 대한 95% 신뢰구간을 구하라.
x <- c(70, 80, 72, 76, 76, 76, 72, 78, 82, 64, 74, 92, 74, 68, 84);x
y <- c(68, 72, 62, 70, 58, 66, 68, 52, 64, 72, 74, 60, 74, 72, 74);y
D <- x-y;D 
n <- length(D);n 
Dbar <- mean(D);Dbar
s <- sd(D);s
se <- s/sqrt(n);se
df <- n-1;df
t_0.025 <- qt(0.025,df,lower.tail=F);t_0.025
Dbar+c(-1,1)*t_0.025*se
# 검정
#  이 자료로부터 약이 혈압을 내린다는 주장을 할 수 있겠는가? 유의수준 1%로 검정하라.
# H0: delta = 0 vs. H1: delta > 0
delta <- 0;delta
T <- (Dbar-delta)/se;T
t_0.01 <- qt(0.01,df,lower.tail=F);t_0.01
T >= t_0.01
# 예제. 어려운 것을 기억하는 두 가지 방법 중 어떤 것이 더 효과적인지 결정하고자 한다. 다음은 IQ와 성적을 기준으로 짝지은 9쌍의 학생들을 대상으로 두 방법을 각 쌍에서 임의로 배정하여 기억력 실험을 실행한 결과이다.
# 유의수준 95%에서 두 방법의 효과의 차이가 유의한지 검정하라.
A <- c(90, 86, 72, 65, 44, 52, 46, 38, 43);A
B <- c(85, 87, 70, 62, 44, 53, 42, 35, 46);B
D <- A-B;D
n <- length(D)
Dbar <- mean(D);Dbar
s <- sd(D);s
se <- s/sqrt(n);se
# H0: delta = 0 vs. H1: delta != 0
t <- (Dbar-delta)/se;t
df <- n-1;df
t_0.025 <- qt(0.025,df,lower.tail=F);t_0.025
abs(t) >= t_0.025

### 모비율의 차에 대한 추론
## 신뢰구간
# 예제. 두 씨앗의 발아율의 차이의 95% 신뢰구간을 구하라.
n1 <- 100;n1
n2 <- 150;n2
x <- 88;x
y <- 126;y
phat_1 <- x/n1;phat_1
phat_2 <- y/n2;phat_2
z_0.025 <- qnorm(0.025,lower.tail=F);z_0.025
se <- sqrt(phat_1*(1-phat_1)/n1+phat_2*(1-phat_2)/n2);se
(phat_1-phat_2)+c(-1,1)*z_0.025*se
## 검정
# 에제. 앞의 예제의 화학적 처리가 씨의 발아비율을 높인다고 할 수 있는지 유의수준 5%로 검정하라.
# H0: p1 = p2 vs. H1: p1 > p2 
phat <- (x+y)/(n1+n2);phat
Z <- (phat_1-phat_2)/sqrt(phat*(1-phat)*(1/n1+1/n2))
z_0.05 <- qnorm(0.05,lower.tail=F);z_0.05
Z >= z_0.05
# 예제. 살균제 리스테린 사용 여부에 따라 절단 수술 환자의 생존율에 차이가 있는지 유의수준 0.05에서 검정하라.
# H0: p1 = p2 vs. H1: p1 != p2
n1 <- 40;n1
n2 <- 35;n2
x <- 34;x
y <- 19;y
phat_1 <- x/n1;phat_1
phat_2 <- y/n2;phat_2
phat <- (x+y)/(n1+n2);phat
Z <- (phat_1-phat_2)/sqrt(phat*(1-phat)*(1/n1+1/n2))
z_0.025 <- qnorm(0.025,lower.tail=F);z_0.025
abs(Z) >= z_0.025
