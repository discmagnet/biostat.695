# Homework 02 R Code

# plot likelihood function
curve(x^20, from = 0, to = 1,
      xlab = expression(pi),
      ylab = expression(paste("L(",pi,")")),
      main = "Likelihood Function")
curve(x^2, from = -1, to = 1, add = T)

