#The following code computes the Wald based confidence interval for the Application on Slide 21, Lecture 2:

    y = 0
    n = 25
    pi.hat = y/n
    pi.hat + c(-1,1)*qnorm(p=0.975)*sqrt(pi.hat*(1-pi.hat)/n)

#The following code computes the score based confidence interval:

scoreCI = prop.test(x=0,n=25,conf.level=0.95,correct=F)
scoreCI$conf.int

#The following code computes the LR based confidence interval using
#function uniroot in R.

LR = function(pi.0,y,N,alpha) {
  pi.hat = y/N; L0 = 0; L1 = 0 
  print(pi.0)
  if (pi.0 < 1) L0 = L0 + (N-y)*log(1-pi.0)
  if (pi.0 > 0) L0 = L0 + y*log(pi.0)
  if (pi.hat > 0) L1 = L1 + y*log(pi.hat)
  if (pi.hat < 1) L1 = L1 + (N-y)*log(1-pi.hat)
  LR = 2*(L1-L0)
  LR-qchisq(1-alpha,df=1)
}

uniroot(f=LR,interval=c(0.000001,0.999999),N=25,y=0,alpha=0.05)$root
# computes the upper bound at level 0.05 with observed successes y = 0 on 25 observations

    #if instead the observed number of successes is 5, we can compute the lower and
    #upper bounds as follows:

x = seq(0.001,.999,.001)
y = rep(0,length(x))
for (i in 1:length(x))
    y[i] = LR(x[i],5,25,.05)
plot(x,y,type="l"); abline(h=0,lty=2,col=2)

uniroot(f=LR,interval=c(0.000001,5/25),N=25,y=5,alpha=0.05)$root # lower bound
uniroot(f=LR,interval=c(5/25,0.999999),N=25,y=5,alpha=0.05)$root # upper bound


#The following function computes the LR based confidence interval using function nlm in R.

binLR.CI = function(y,n,start,alpha, ...){
   LR = function(pi.0,y,N,alpha) {
   pi.hat = y/N; L0 = 0; L1 = 0 
   if (pi.0 < 1) L0 = L0 + (N-y)*log(1-pi.0)
   if (pi.0 > 0) L0 = L0 + y*log(pi.0)
   if (pi.hat > 0) L1 = L1 + y*log(pi.hat)
   if (pi.hat < 1) L1 = L1 + (N-y)*log(1-pi.hat)
   LR = 2*(L1-L0)
   (LR-qchisq(1-alpha,df=1))^2
  }
  nlm(f=LR,p=start,y=y,N=n,alpha=alpha,...)
}


binLR.CI(0,25,.2,0.05)$estimate # computes the upper bound at level 0.05 with observed successes y = 0 on 25 observations

    #if instead the observed number of successes is 5, we can compute the lower and
    #upper bounds as follows:

x = seq(0.001,.999,.001)
y = rep(0,length(x))
for (i in 1:length(x))
    y[i] = LR(x[i],5,25,.05)^2
plot(x,y,type="l",ylim=c(-1,30))
abline(h=0,lty=2,col=2)

binLR.CI(5,25,.1,0.05)$estimate # lower bound
binLR.CI(5,25,.4,0.05)$estimate # upper bound

