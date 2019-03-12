# Homework 5 R Code

# Problem 2 - Snoring
snore = read.table("snoring.txt",header=T)
# part (a)
snore$score_a <- c(0,2,4,6)
snore.ungrp.a = data.frame(snore=c(rep(snore$score_a,times=snore$yes),
                                 rep(snore$score_a,times=snore$no)),
                         hd=c(rep(1,times=sum(snore$yes)),
                              rep(0,times=sum(snore$no))))
snore.fit.ungrp.a = glm(hd~snore,data=snore.ungrp.a,family=binomial())
summary(snore.fit.ungrp.a)
# part (b)
snore$score_b <- c(0,1,2,3)
snore.ungrp.b = data.frame(snore=c(rep(snore$score_b,times=snore$yes),
                                   rep(snore$score_b,times=snore$no)),
                           hd=c(rep(1,times=sum(snore$yes)),
                                rep(0,times=sum(snore$no))))
snore.fit.ungrp.b = glm(hd~snore,data=snore.ungrp.b,family=binomial())
summary(snore.fit.ungrp.b)
# part (c)
snore$score_c <- c(1,2,3,4)
snore.ungrp.c = data.frame(snore=c(rep(snore$score_c,times=snore$yes),
                                   rep(snore$score_c,times=snore$no)),
                           hd=c(rep(1,times=sum(snore$yes)),
                                rep(0,times=sum(snore$no))))
snore.fit.ungrp.c = glm(hd~snore,data=snore.ungrp.c,family=binomial())
summary(snore.fit.ungrp.c)

# Problem 3
x <- c(0,1,2)
n <- c(4,4,4)
s <- c(1,2,4)
f <- c(3,2,0)
data <- data.frame(x,n,s,f)
data.ungrp <- data.frame(x=c(rep(data$x,times=data$n)),
                         s=c(rep(1,1),rep(0,3),
                             rep(1,2),rep(0,2),
                             rep(1,4)))
# data file (i) ungrouped
fit.ungrp.0 = glm(s~1,data=data.ungrp,family=binomial())
summary(fit.ungrp.0)
fit.ungrp.1 = glm(s~x,data=data.ungrp,family=binomial())
summary(fit.ungrp.1)
# L0
fit.ungrp.0$deviance
logLik(fit.ungrp.0)
# L1
fit.ungrp.1$deviance
logLik(fit.ungrp.1)
# data file (ii) grouped
fit.grp.0 = glm(cbind(s,f)~1,data=data,family=binomial())
summary(fit.grp.0)
fit.grp.1 = glm(cbind(s,f)~x,data=data,family=binomial())
summary(fit.grp.1)
# L0
fit.grp.0$deviance
logLik(fit.grp.0)
# L1
fit.grp.1$deviance
logLik(fit.grp.1)