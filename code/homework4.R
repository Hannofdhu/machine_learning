library(MASS)
summary(Boston)

set.seed(1)
attach(Boston)
medv.mean=mean(medv);medv.mean
#22.533

Medv.err=sd(medv)/sqrt(length(medv));Medv.err
#0.4088611

Boot.fn=function(data,index)return (mean(data[index]))
library(boot)
Bstrap=boot(medv,Boot.fn,1000);Bstrap

# Bootstrap Statistics :
#     original      bias    std. error
# t1* 22.53281 0.005601581   0.4060352


t.test(medv)
c(bstrap$t0-2*0.4107,bstrap$t0+2*0.4107)

# One Sample t-test
# data:  medv
# t = 55.111, df = 505, p-value < 2.2e-16
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#  21.72953 23.33608
# sample estimates:
# mean of x 
#  22.53281 

Medv.med=median(medv);Medv.med
#21.2

Boot.fn=function(data,index) return(median(data[index]))
Boot(medv,Boot.fn,1000)

