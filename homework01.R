# Homework 01 R Code

# write a function which will be used as input to the uniroot() function
score = function(data, mu.0, alpha){
  N <- length(data)
  U <- sum(data)/mu.0 - N # score function of a Poisson distribution
  I <- N/mu.0 # expected information of a Poisson distribution
  score <- U*U/I # compute score statistic
  score - qchisq(1-alpha, df=1) # determine rejection region
}

# define data
data <- c(4,9,6,3,6,7,4,5,1,6)
mu.hat <- mean(data)

# determine upper and lower bounds of the 95% CI
uniroot(f = score, interval = c(3,mu.hat), data = data, alpha = 0.05)$root
uniroot(f = score, interval = c(mu.hat,8), data = data, alpha = 0.05)$root

# plot the function for values of mu between 3 and 8
x <- seq(3,8,.1)
y <- rep(0,length(x))
for (i in 1:length(x))
  y[i] = score(data, x[i], .05)
plot(x,y,type="l"); abline(h=0,lty=2,col=2)