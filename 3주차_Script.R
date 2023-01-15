rm(list=ls())

# 모비율의 점추정
# 어느 회사의 신제품 구입 희망 고객 비율
# 어느 회사가 전체 고객명단에서 임의로 250명을 추출하여 신상품 안내책자를 발송한 결과, 그 중 70명의 고객이 구입을 희망하였다.
# 이 표본 자료에 근거해서 전체 고객 중 신상품 구입을 원하는 고객의 모비율(p)과 추정량의 표준오차를 구하라.
x <- 70;x
n <- 250;n
phat <- x/n;phat
se_phat <- sqrt(phat*(1-phat)/n);se_phat

# 모비율의 구간추정
# 실업률의 추정
# 어느 도시의 노동인구 500명 중 41명이 실업자라고 한다. 이 자료로부터 도시 전체의 실업률 p에 대한 95% 신뢰구간을 구하라.
n <- 500;n
x <- 41;x
phat <- x/n;phat
se_phat <- sqrt(phat*(1-phat)/n);se_phat
z_0.025 <- qnorm(0.025,lower.tail=F);z_0.025
c(phat-z_0.025*se_phat,phat+z_0.025*se_phat)

# 모비율의 가설검정
# 방사선 치료를 병행한 환자의 암 완치율
# 어떤 특정한 암의 경우 수술을 시행한 후 완치되는 비율(5년 이상 생존비율)이 30%라고 한다. 이 암에 걸린 60명의 환자를 대상으로 수술과 더불어 방사선 치료를 병행하였더니 60명 중 27명이 완치되었다고 한다. 수술과 방사선치료를 병행하는 것이 완치율을 높이는데 효과가 있는지 검정하고자 한다. 적절한 가설을 세우고, 유의수준 5%에서 검정하라.
# H0: p = 0.3 vs. H1: p > 0.3
n <- 60;n
x <- 27;x
phat <- x/n;phat
p0 <- 0.3;p0
Z <- (phat-p0)/sqrt(p0*(1-p0)/n);Z
pnorm(Z,lower.tail=F) <= 0.05
Z >= qnorm(0.05,lower.tail=F)
# 즉, 방사선 치료를 병행하는 것이 암의 완치율을 높이는데 효과가 있다고 할 수 있다

# 어느 대학교의 총학생회장 선거에서의 투표율을 알아보기 위해서 78명의 학생을 대상으로 조사한 결과 49명이 투표에 참가하였다고 하자. 총학생회장 선거의 투표율을 추정하고 추정량의 표준오차와 모비율에 대한 95% 신뢰구간을 제시하라.
n <- 78;n
x <- 49;x
phat <- x/n;phat
se_phat <- sqrt(phat*(1-phat)/n);se_phat
z_0.025 <- qnorm(0.025,lower.tail=F);z_0.025
c(phat-z_0.025*se_phat,phat+z_0.025*se_phat)

# 어떤 생산공정에서 생산품의 불량률이 2%를 초과하면 관리이탈로 간주된다. 어떤 한 주 동안 생산된 제품 중 500개의 제품을 검사하였더니 13개가 불량이었다고 할 때, 이 결과가 그 주 동안 관리이탈상태였다고 주장할 수 있는 강한 이유가 될 수 있겠는가? 적절한 가설을 세우고 유의수준 5%에서 검정하라
# H0: p = 0.02 vs. H1: p > 0.02
n <- 500;n
x <- 13;x
phat <- x/n;phat
p0 <- 0.02;p0
Z <- (phat-p0)/sqrt(p0*(1-p0)/n);Z
pnorm(Z,lower.tail=F) <= 0.05
Z >= qnorm(0.05,lower.tail=F)
# 즉, 불량률이 2%를 초과하였다고 할 수 없으므로 관리이탈상태였다고 주장할 수 없다

# 실제 데이터
# iris 꽃의 종 중 setosa의 비율
data(iris)
iris
dat <- iris

head(iris)
table(dat$Species)
# H0: p = 0.3 vs. H1: p < 0.3
n <- dim(dat)[1];n
x <- 50;x

phat <- x/n;phat
se_phat <- sqrt(phat*(1-phat)/n);se_phat

z_0.005 <- qnorm(0.005,lower.tail=F);z_0.005
c(phat-z_0.005*se_phat,phat+z_0.005*se_phat) # 99% C.I.
round(c(phat-z_0.005*se_phat,phat+z_0.005*se_phat),2)

p0 <- 0.3;p0
Z <- (phat-p0)/sqrt(p0*(1-p0)/n);Z
pnorm(Z) <= 0.01
Z <= qnorm(0.01) 
