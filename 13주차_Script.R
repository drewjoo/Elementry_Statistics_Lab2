dat <- data.frame(y=c(10,15,8,12,15,
                      14,18,21,15,
                      17,16,14,15,17,15,18,
                      12,15,17,15,16,15),
                  coating=c(rep('A',5),rep('B',4),
                            rep('C',7),rep('D',6)));dat

T1 <- sum(dat$y[dat$coating=='A']);T1
T2 <- sum(dat$y[dat$coating=='B']);T2
T3 <- sum(dat$y[dat$coating=='C']);T3
T4 <- sum(dat$y[dat$coating=='D']);T4
Total <- sum(dat$y);Total

n <- length(dat$y);n
n1 <- length(dat$y[dat$coating=='A']);n1
n2 <- length(dat$y[dat$coating=='B']);n2
n3 <- length(dat$y[dat$coating=='C']);n3
n4 <- length(dat$y[dat$coating=='D']);n4

SST <- sum(dat$y^2)-Total^2/n;SST
SStr <- sum(c(T1^2/n1,T2^2/n2,T3^2/n3,T4^2/n4))-Total^2/n;SStr
SSE <- sum(dat$y^2)-sum(c(T1^2/n1,T2^2/n2,T3^2/n3,T4^2/n4));SSE

k <- 4;k
df_SST <- n-1;df_SST
df_SStr <- k-1;df_SStr
df_SSE <- n-k;df_SSE

MStr <- SStr/df_SStr;MStr
MSE <- SSE/df_SSE;MSE

F_stat <- MStr/MSE;F_stat

F_stat >= qf(0.05,df_SStr,df_SSE,lower.tail = F) # 기각

p_value <- pf(F_stat,df_SStr,df_SSE,lower.tail = F);p_value

m <- aov(y~coating,data=dat);m
summary(m)

