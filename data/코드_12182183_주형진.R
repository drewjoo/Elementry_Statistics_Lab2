setwd("C:/Users/Administrator/OneDrive/바탕 화면/기통실2/data")

##Question1##
carbon<-read.csv("carbon.csv")

#1.산점도
plot(x=carbon$x,y=carbon$y)
#2.추정회귀선
fit<-lm(data=carbon, y~x)
coef(fit) # y = -65.7197811 + 0.3440518*x
abline(fit, col="red", lwd=1.2)

#3. 
#H0: beta_1 = 0 
#H1: beta_1 >0
tobs=qt(0.025, df=fit$df.residual, lower.tail = F);tobs
Sxx=sum((carbon$x-mean(carbon$x))^2);Sxx
t<-(fit$coef[2]-0)/(sigma(fit)/sqrt(Sxx));t
t>tobs
summary(fit)$coefficient[2,3]>tobs
#t 검정통계량이 기준치 보다 크기 때문에 주장을 뒷받침 할 수 있다. 

#4. 신뢰구간(confidence level)
predict(fit, interval = "confidence",method="confidence",newdata = data.frame(x=300),level=0.95)
# 32.75449 <= x <= 42.23703

#5 SST,SSE,SSR,R^2
summary(fit)
SSE=sum(resid(fit)^2)
SST=sum((carbon$y-mean(carbon$y))^2)
SSR=SST-SSE
rbind(c(SST=SST,SSE=SSE,SSR=SSR))
R_squared=SSR/SST;R_squared 

#6. Residuals vs Fitted
plot(resid(fit)~fitted(fit))
plot(fit, which = 1)
#그림에서 특별한 형태 없이 잘 산개해 있다고 볼 수 있다.

##Question2##
textile<-read.csv("textile.csv")

dat=data.frame(y=as.numeric(as.matrix(textile)),textile=rep(1:4,each=5))
m=aov(y~textile, data=dat)
summary(m)
#P-value가 0.0038로 0.05보다 작기 때문에 귀무가설을 기각하고 대립가설을 채택할 수 있다. 
#즉 인화성이 textile마다 서로 다르다고 볼 수 있다. 



##Question3##
#1.
blood<-read.csv("blood.csv")
O<-blood[2:5]
E<-c(0.25,0.25,0.25,0.25)*sum(O)
Chi=sum(((O-E)^2)/E);Chi
Chi>qchisq(0.05, df=3, lower.tail = F)
#비율이 모두 같다는 귀무가설을 기각할 수 있다.

#2.
O<-read.csv("blood.csv")[2:5]
E<-c(0.4,0.4,0.1,0.1)*sum(O)
Chi=sum(((O-E)^2)/E);Chi
Chi>qchisq(0.05, df=3, lower.tail = F)
#비율이 4:4:1:1 이라는 귀무가설을 기각할 수 있다. 

##Question4##
goods<-read.csv("goods.csv")
col_total<-goods[4,2:4]
n<-sum(col_total)
E1=goods$total[1]*col_total/n
E2=goods$total[2]*col_total/n
E3=goods$total[3]*col_total/n
Chi=sum((goods[1,2:4]-E1)^2/E1,(goods[2,2:4]-E2)^2/E2,(goods[3,2:4]-E3)^2/E3);Chi
Chi>qchisq(0.05, df=4, lower.tail = F)

chisq.test(goods[1:3,2:4])
chisq.test(goods[1:3,2:4])$p.value<=0.05
#귀무가설을 기각하고 대립가설을 채택할 수 있다.
#즉, 도시에 따른 결과에 차이가 있다고 볼수 있다. 


##Question5##
olive<-read.csv("olive.csv")
olive<-data.frame(olive[,2],olive[,3]-olive[,2])
colnames(olive)=c("likes","dislikes");rownames(olive)<-c("W","E","S","M")

n<-sum(olive)
col_total<-apply(olive, 2, sum)
row_total<-apply(olive,1,sum)
E_W=n*(row_total[1]/n)*(col_total/n)
E_E=n*(row_total[2]/n)*(col_total/n)
E_S=n*(row_total[3]/n)*(col_total/n)
E_N=n*(row_total[4]/n)*(col_total/n)

Chi=sum((olive[1,]-E_W)^2/E_W+(olive[2,]-E_E)^2/E_E+(olive[3,]-E_S)^2/E_S+(olive[4,]-E_N)^2/E_N);Chi
Chi>=qchisq(0.05, df=3,lower.tail = F)

chisq.test(olive)
chisq.test(olive)$p.value<=0.05
#검정통계량의 값이 기준치보다 작고 p-value도 0.05보다 크다
#귀무가설을 기각할수 없다. 
#따라서 출신지와 올리브 선호도는 독립이라고 볼 수 있다. 