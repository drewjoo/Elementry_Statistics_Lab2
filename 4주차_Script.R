#기초통계 실습 week 4

## ex1.
alpha=0.1
# 자유도가 2일 떄의 상위 10%의 확률을 주는 값
qt(1-alpha, df=2)

#자유도가 10일때의 상위 10%의 확률률을 주는 값
qt(1-alpha, df=10)


## ex2. 
alpha=0.1
#자유도가 9인 t분포에서 5%의 확률을 주는 값
qt(1-alpha/2, df=9)


## ex3.
n<-15
x_bar<-39.3
s<-2.6
se<-s/sqrt(n)
alpha=0.1
t<-qt(1-alpha/2, df=n-1)
c(x_bar-t*se, x_bar+t*se)


## ex4. 
# H0 : mu = 200 vs H1 : mu <200
data <- c(175, 190, 215, 198, 184, 207, 210, 193, 196, 180)
n<-length(data)
x_bar=mean(data)
s<-sd(data)
se<-s/sqrt(n)

t_val<-(x_bar-200)/se
t_val

R<-qt(1-0.05, df=n-1)
R
abs(t_val)>R

p_val<-pt(abs(t_val), df=n-1, lower.tail = F)
p_val<0.05


## ex5. 
n<-9
x_bar<-8.3
s<-1.2
se<-s/sqrt(n)
alpha<-0.05
t<-qt(1-alpha/2, df=n-1)

# 95% 신뢰구간 
c(x_bar-t*se, x_bar+t*se)

# t 검정
t_val<-(x_bar-8.5)/se
abs(t_val)>t

pt(abs(t_val), df=n-1, lower.tail = F)

