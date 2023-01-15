## 예제1 
obs <- c(18,55,27);obs
n <- sum(obs);n

null_probs <- c(1/4,1/2,1/4);null_probs
expected <- n*null_probs;expected

Chi <- sum(((obs-expected)^2)/expected);Chi

k <- 3;k
Chi >= qchisq(0.05,k-1,lower.tail=F) # 기각 X
# 즉, 주어진 자료로부터 생물학의 이론이 제안한 모집단의 비율이 틀리다는 결론을 내릴 수 없다.

chisq.test(obs,p=null_probs)

## 예제2
A <- c(37,24,19);A
B <- c(17,33,20);B

col_total <- A+B;col_total
n <- sum(col_total);n
estimated_prob <- col_total/n;estimated_prob

n_A <- sum(A);n_A
n_B <- sum(B);n_B

expected_A <- n_A*estimated_prob;expected_A
expected_B <- n_B*estimated_prob;expected_B

Chi <- sum((A-expected_A)^2/expected_A + (B-expected_B)^2/expected_B);Chi

Chi >= qchisq(0.05,1*2,lower.tail = F) # 기각
# 즉, 두 식이요법 간의 차이는 통계적으로 유의하다고 볼 수 있다. 데이터를 보게 되면 '양호'의 범주에서 식아요법 'A'가 'B' 보다 월등함을 볼 수 있다.

dat <- rbind(A,B);dat
chisq.test(dat)

## 예제3
male <- c(378,237,26);male
female <- c(388,196,25);female

col_total <- male+female;col_total
n <- sum(col_total);n

n_m <- sum(male);n_m
n_f <- sum(female);n_f

expected_m <- n * (n_m/n) * (col_total/n);expected_m
expected_f <- n * (n_f/n) * (col_total/n);expected_f

Chi <- sum(((male-expected_m)^2/expected_m)+((female-expected_f)^2/expected_f));Chi

Chi >= qchisq(0.05,1*2,lower.tail = F) # 기각 X
# 즉, 오락물 방영에 대한 의견과 성별과는 어떤 관계가 있다고 말할 수 없다.

dat <- rbind(male,female);dat
chisq.test(dat)
