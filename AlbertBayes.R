### ver.6 # 2020 01 05 # 2011 11 28 # 2012 08 14 # 2020 12 09
# This script file is encoded in UTF-8 (Mac)

############################################################
#                       アルバート著                         #
#                 『Rで学ぶベイズ統計学』                    #
#                   石田基広・石田和枝訳                     #
#                   丸善出版                                #
############################################################

############################################################
#
#           これは邦訳版読者用のスクリプトです．             #
#     原著に掲載されたコードとは異なる場合があります．        #

#               ishida.motohiro @ gmail.com                #
############################################################

## studentdata <- read.table("studentdata.txt", header = TRUE)# sep = "\t", 


# install.packages("LearnBayes")


library(LearnBayes)
####################################
# Section 1.2.2  R にデータを読み込む
#   Reading the Data into R
####################################

data(studentdata)

studentdata[1,]

attach(studentdata)


####################################
# Section 1.2.3 グループ別に要約しグラフにするためのR 関数
#  R Commands to Summarize and Graph a Single Batch
####################################

table(Drink)
tabDr <- table(Drink)
barplot(tabDr)

(hours.of.sleep <- WakeUp - ToSleep)

plot (ToSleep, hours.of.sleep)
plot (jitter(ToSleep), jitter(hours.of.sleep))
summary(hours.of.sleep)
hist(hours.of.sleep,main="")


####################################
# Section 1.2.4 グループ同士を比較するための R 関数
#   R Commands to Compare Batches
####################################

boxplot(hours.of.sleep ~ Gender,
  ylab = "Hours of Sleep") #睡眠時間

female.Haircut <- Haircut[Gender=="female"]
male.Haircut <- Haircut[Gender=="male"]
summary(female.Haircut)
summary(male.Haircut)


####################################
# Section 1.2.5 関連を調べるためのR 関数
#    R Commands for Studying Relationships
####################################

plot(jitter(ToSleep), jitter(hours.of.sleep))

fit <- lm(hours.of.sleep ~ ToSleep)
fit
abline(fit)


####################################
# Section 1.3 t 統計量の頑健性を探る
#    Exploring the Robustness of the t Statistic
####################################


####################################
# Section 1.3.2 t 統計量を計算するための関数を作成する
#   Writing a Function to Compute the t Statistic
####################################


x <- rnorm(10, mean = 50, sd = 10)
y <- rnorm(10, mean = 50, sd = 10)
m <- length(x)
n <- length(y)
sp <- sqrt(((m-1) * sd(x)^2+(n-1) * sd(y)^2) / (m+n-2))
t.stat <- (mean(x) - mean(y)) / (sp * sqrt(1/m + 1/n))



tstatistic <- function(x,y){
  m <- length(x)
  n <- length(y)
  sp <- sqrt(((m - 1) * sd(x)^2 + (n - 1) * sd(y)^2) / (m + n - 2))
  tstatic <- (mean(x) - mean(y)) / (sp * sqrt(1/m + 1/n))
  return(tstatic )
}

# source("tstatistic.R")

data.x <- c(1, 4, 3, 6, 5)
data.y <- c(5, 4, 7, 6, 10)
tstatistic(data.x, data.y)

####################################
# Section 1.3.3 モンテカルロシミュレーションを実装する
#    Programming a Monte Carlo Simulation
####################################

alpha <- .1; m <- 10; n <- 10;
N <- 10000
n.reject <- 0


tstat <- vector(mode = "numeric", 100) #as.numeric(100)

N <- 10000; m <- n <- 10; alpha <- 0.1; n.reject <- 0

for(i in 1:N){
  x <- rnorm(m, mean = 10, sd = 2)
  y <- rexp(n, rate = 1/10 )
  tstat[i] <- tstatistic(x,y)
  if(abs(tstat[i]) > qt(1 - alpha/2, n+m-2))
    n.reject <- n.reject + 1
}

true.sig.level <- n.reject/N #



####################################
# 1.3.4 異なる仮説下での真の有意水準の挙動
#   The Behavior of the True Significance Level Under Different Assumptions
####################################

m <- 10; n <- 10
my.tsimulation <- function()
 tstatistic(rnorm(m, mean = 10, sd = 2), rexp(n, rate = 1/10))

tstat.vector <- replicate(10000, my.tsimulation())

plot(density(tstat.vector), xlim = c(-5, 8), ylim = c(0,.4), lwd = 3)
curve(dt(x, df = 18), add = TRUE)
legend(4, .3, c("exact", "t(18)"), lwd = c(3, 1))



# 1.6

## hist(table(Dvds))

## length(Dvds); 
## sum(Dvds  > 100, na.rm = T)
## barplot(table(Dvds))
## names( studentdata )

## dvd.lm <- lm(hours.of.sleep ~ Dvds)
## summary(dvd.lm)

## dvd.lm <- lm(hours.of.sleep ~ Dvds)
## summary(dvd.lm)


## plot(density(tstat), xlim = c(-5, 8), ylim = c(0, .4), lwd = 3)
## x <- seq(-5, 5, 0.1)
## lines(x, dt(x, 18))


###############################################################
#
#
#                        第 2 章
#
#
###############################################################

# p.20 下で出てくる尤度関数の意味は，たとえば
# 「ベイズ統計学入門」繁枡算男 p. 39 - 40 の記述を参照
library(LearnBayes)

####################################
# Section 2.3 離散的事前分布の利用
#   Using a Discrete Prior
####################################

p <- seq(0.05, 0.95, by = 0.1)
# prior <- c(2,4,8,8,4,2,1,1,1,1)
prior <- c(1, 5.2, 8, 7.2, 4.6, 2.1, 0.7, 0.1, 0, 0)
prior <- prior/sum(prior)
plot(p, prior, type = "h", ylab = "Prior Probability")
# plot(p, prior, type = "h", ylab = "事前確率")

dat <- c(11, 16)
post <- pdisc(p, prior, dat)
round( cbind(p,prior, post), 2 )
sum(post[3:5])


library(lattice)


## PRIOR <- data.frame("prior",p,prior)
## POST <- data.frame("posterior",p,post)
## names(PRIOR) <- c("Type","P","Probability")
## names(POST) <- c("Type","P","Probability")
## data=rbind(PRIOR,POST)
## xyplot(Probability~P|Type,data=data,layout=c(1,2),
##    type="h",lwd=3,col="black")

PRIOR <- data.frame("事前",p,prior)
POST <- data.frame("事後",p,post)
names(PRIOR) <- c("Type","P","Probability")
names(POST) <- c("Type","P","Probability")
data=rbind(PRIOR,POST)
xyplot(Probability~P|Type,data=data,layout=c(1,2),
   type="h",lwd=3,col="black")



################################
# Section 2.4 ベータ事前分布の利用
#  Using a Beta Prior
#############################


quantile1 <- list(p = .9, x = .5)
quantile2 <- list(p = .5, x = .3)
beta.select(quantile1, quantile2)


a <- 3.26
b <- 7.18
s <- 11
f <- 16
 curve(dbeta(x,a+s,b+f), from=0, to=1, 
       xlab="p",ylab="Density",lty=1,lwd=4)
 curve(dbeta(x,s+1,f+1),add=TRUE,lty=2,lwd=4)
 curve(dbeta(x,a,b),add=TRUE,lty=3,lwd=4)
##  legend(.7,4,c("Prior","Likelihood","Posterior"),
##      lty=c(3,2,1),lwd=c(3,3,3))
 legend(.7,4,c("事前","尤度","事後"),
     lty=c(3,2,1),lwd=c(3,3,3))


1 - pbeta(0.5, a + s, b + f)



qbeta(c(0.05, 0.85), a + s, b + f)

ps <- rbeta(1000, a + s, b + f)
hist(ps, xlab = "p", main = "")



sum(ps >= 0.5) / 1000
quantile(ps, c(0.05, 0.95))



#####################################
# Section 2.5 ヒストグラム事前分布の利用
#  Using a Histogram Prior
#####################################


midpt <- seq(0.05, 0.95, by = 0.1)
prior <- c(1, 5.2, 8, 7.2, 4.6, 2.1, 0.7, 0.1, 0, 0)
prior <- prior/sum(prior)
p <- seq(0, 1, length =500)

curve(histprior(x,midpt,prior), from = 0, to = 1,
#      ylab="Prior density",ylim=c(0,.3))
      ylab = "事前密度", ylim = c(0,.3))

s <- 11; f <- 16

curve( histprior(x,midpt,prior) * dbeta(x, s + 1, f + 1),
      from = 0, to = 1,
#      ylab="Prior density")
      ylab = "事前密度" )



p <- seq(0, 1, length = 500)
post <- histprior(p, midpt, prior) * dbeta(p, s + 1, f + 1)
post <- post/sum(post)


library(LearnBayes)

midpt <- seq(0.05, 0.95, by = 0.1)
prior <- c(1, 5.2, 8, 7.2, 4.6, 2.1, 0.7, 0.1, 0, 0)
prior <- prior/sum(prior)
 
##  curve(histprior(x,midpt,prior), from=0, to=1,
##    ylab="Prior density",ylim=c(0,.3))
curve(histprior(x,midpt,prior), from=0, to=1,
   ylab="離散密度",ylim=c(0,.3))


 s <- 11
 f <- 16

 ## curve(histprior(x,midpt,prior) * dbeta(x,s+1,f+1), 
##    from=0, to=1, ylab="Posterior density")
 curve(histprior(x,midpt,prior) * dbeta(x,s+1,f+1), 
   from=0, to=1, ylab = "事後密度")

 p = seq(0, 1, length=500)
 post = histprior(p, midpt, prior) *
        dbeta(p, s+1, f+1)
 post = post/sum(post)
 ps = sample(p, replace = TRUE, prob = post)

 hist(ps, xlab = "p", main = "")


################################################
# Section 2.6 予測
#  Prediction
################################################

 library(LearnBayes)

 p <- seq(0.05, 0.95, by=.1)
 prior <- c(1, 5.2, 8, 7.2, 4.6, 2.1, 0.7, 0.1, 0, 0)
 prior <- prior/sum(prior)
 m <- 20; ys <- 0:20
 pred <- pdiscp(p, prior, m, ys)
 cbind(0:20,pred)

 ab <- c(3.26, 7.19)
 m <- 20; ys <- 0:20
 pred <- pbetap(ab, m, ys)

 p <- rbeta(1000,3.26, 7.19)

 y <- rbinom(1000, 20, p)

 table(y)

 freq <- table(y)
 ys <- as.integer(names(freq))
 predprob <- freq/sum(freq)
 plot(ys, predprob, type = "h", xlab = "y",
   ylab = "Predictive Probability")

 dist <- cbind(ys,predprob)

 covprob <- .9
 discint(dist, covprob)



###############################################################
#
#
#                        第３章
#
#

######################################################################
# Section 3.2 平均が既知で分散が未知の正規分布
#        Normal Distribution with Known Mean but Unknown Variance
######################################################################

library(LearnBayes)
data(footballscores)
attach(footballscores)

d <- favorite - underdog - spread
n <- length(d)
v <- sum(d^2)

P <- rchisq(1000, n) / v
s <- sqrt(1/P)
hist(s, main = "")


##########################################################
# Section 3.3 心臓移植手術の死亡率を推定する
#       Estimating a Heart Transplant Mortality Rate
##########################################################


my.alpha <- 16
my.beta <- 15174

yobs <- 1
ex <- 66
y <- 0:10
lam <- my.alpha/my.beta

py <- dpois(y, lam*ex)*dgamma(lam, shape = my.alpha, rate = my.beta) / dgamma(lam, shape = my.alpha + y, rate = my.beta + ex)
cbind(y, round(py, 3))

lambdaA <- rgamma(1000, shape = my.alpha + yobs, rate = my.beta + ex)
## hist(lambdaA)
## mean (lambdaA)

yobs <- 4
ex <- 1767
py <- dpois(y, lam*ex)*dgamma(lam, shape = my.alpha, rate = my.beta)/ dgamma(lam, shape = my.alpha + y, rate = my.beta + ex)
cbind(y, round(py, 3))
lambdaB <- rgamma(1000, shape = my.alpha + yobs, rate = my.beta + ex)

lambda <- seq(0, max(c(lambdaA, lambdaB)), length = 500)





par(mfrow = c(2,1)) 
#
hist(lambdaA, freq = FALSE, main = "", ylim =c(0, 1600))
lines(lambda, dgamma(lambda, shape = my.alpha, my.beta),col = "blue", lwd = 3)
lines(lambda, dgamma(lambda, shape = my.alpha+ 1, my.beta + 66), col = "red", lwd = 3)
legend(0.0015, 1500, legend= c("prior", "posterior"), col =  c("blue","red"), lwd = 3)

hist(lambdaB, freq = FALSE, main = "", ylim =c(0, 1600))
lines(lambda, dgamma(lambda, shape = my.alpha, my.beta),col = "blue", lwd = 3)
lines(lambda, dgamma(lambda, shape = my.alpha+ 4, my.beta + 1767), col = "red", lwd = 3)
legend(0.0015, 1500, legend= c("prior", "posterior"), col =  c("blue","red"), lwd = 3)




####################################################
# Section 3.4 ベイズ法の頑健性について
#         An Illustration of Bayesian Robustness
####################################################


library(LearnBayes)
quantile1 <- list(p = .5, x = 100); quantile2 <- list(p = .95, x = 120)
normal.select(quantile1, quantile2)
normal.select

20/qnorm(0.95)
pnorm(80, mean = 100, sd = 12.16); pnorm(120, mean = 100, sd = 12.16)


mu <- 100
tau <- 12.16
sigma <- 15
n <- 4
se <- sigma/sqrt(4)
ybar <- c(110, 125, 140)
tau1 <- 1/sqrt(1/se^2 + 1/tau^2)
mu1 <- (ybar/se^2 + mu/tau^2) * tau1^2

(summ1 <- cbind(ybar, mu1, tau1))

qt(0.05, df=2); qt(0.95, df=2)

tscale <- 20/qt(0.95, 2)
tscale


par(mfrow = c(1,1))
curve(1/tscale * dt((x-mu) / tscale, 2),
      from = 60, to = 140, xlab = "theta", ylab = "Prior Density")
curve(dnorm(x, mean = mu,sd = tau), add = TRUE, lwd = 3 )
legend("topright", legend = c("t density","normal density"), lwd = c(1, 3))




norm.t.compute <- function(ybar) {
     theta = seq(60, 180, length = 500)
     like = dnorm(theta,mean=ybar,sd=sigma/sqrt(n))
     prior = dt((theta - mu)/tscale, 2) # mu = 100, tscale = 20/qt(0.95, 2)
     post = prior * like
     post = post/sum(post)
     m = sum(theta * post)
     s = sqrt(sum(theta^2 * post) - m^2)
     c(ybar, m, s) }
summ2 <- t(sapply(c(110, 125, 140),norm.t.compute))
dimnames(summ2)[[2]]=c("ybar","mu1 t","tau1 t")
summ2



## summ2 <- c()

## for(i in 1:3){
##   theta <- seq(60, 180, length = 500)
##   like <- dnorm((theta - ybar[i]) / se)# se = 7.5
##   prior <- dt((theta -mu) / tscale,2)
##   post <- prior * like
##   post <- post / sum(post)
##   m <- sum(theta * post)
##   s <- sqrt(sum(theta^2 * post) - m^2)
##   summ2 <- rbind(summ2, c(ybar[i],m ,s))
## }
## summ2 
cbind(summ1, summ2)






theta <- seq(60, 180, length=500)
normpost <- dnorm(theta, mu1[3], tau1)
normpost <- normpost/sum(normpost)


plot(theta, normpost, type="l", lwd=3, ylab = "Posterior Density")
like <- dnorm(theta, mean = 140,sd = sigma/sqrt(n))
prior <- dt((theta - mu)/tscale, 2)
tpost <- prior * like / sum(prior * like)  
lines(theta,tpost)
legend("topright", legend = c("t prior","normal prior"), lwd = c(1, 3))



#######################################################
# Section 3.5 ベイズ法の頑健性について
#      Mixtures of Conjugate Priors
#######################################################



probs <- c(.5,.5)
beta.par1 <- c(6, 14)
beta.par2 <- c(14, 6)
betapar <- rbind(beta.par1, beta.par2)
data <- c(7,3)
post <- binomial.beta.mix(probs, betapar, data)
post


curve(.5 * dbeta(x, 6, 14) + .5 * dbeta(x, 14, 6), 0, 1)




## curve(post$probs[1]*dbeta(x,13,17)+post$probs[2]*dbeta(x,21,9),
##  from=0, to=1, lwd=3, xlab="P", ylab="DENSITY")
## curve(.5*dbeta(x,6,12)+.5*dbeta(x,12,6),0,1,add=TRUE)
## legend("topleft",legend=c("Prior","Posterior"),lwd=c(1,3))

curve(post$probs[1] * dbeta(x,13,17) + post$probs[2] * dbeta(x,21,9),
 from = 0, to = 1, lwd = 3, xlab = "P", ylab = "DENSITY")
curve(.5 * dbeta(x,6,14) + .5 * dbeta(x, 12, 4), 0, 1, add = TRUE)
legend("topleft", legend = c("Prior","Posterior"), lwd = c(1,3))




#######################################################
# Section 3.6 コインの偏りについてのベイズ検定
#     A Bayesian Test of the Fairness of a Coin
#######################################################

pbinom(5, 20, 0.5)


n <- 20
y <- 5
a <- 10
p <- 0.5
m1 <- dbinom(y, n, p) * dbeta(p, a, a)/dbeta(p, a + y, a + n - 
     y)
lambda <- dbinom(y, n, p)/(dbinom(y, n, p) + m1)
lambda


 pbetat(p, .5, c(a, a), c(y, n-y))

prob.fair <- function(log.a)
 {
  a = exp(log.a)
  print(a)
  m2 = dbinom(y, n, p) * dbeta(p, a, a)/
              dbeta(p, a + y, a + n - y)
  dbinom(y, n, p)/(dbinom(y, n, p) + m2)
 }


n <- 20; y <- 5; p <- 0.5
curve(prob.fair(x), from = -4, to = 5, xlab = "log a", 
  ylab="コインの偏りの確率", lwd=2)

## curve(prob.fair(x), from = -4, to = 5, xlab = "log a", 
##    ylab = "Prob(coin is fair)", lwd = 2)



n <- 20
y <- 5
a <- 10
p <- .5
m2 <- 0
for (k in 0:y)
  m2 <- m2 + dbinom(k,n,p) * dbeta(p,a,a) / dbeta(p,a+k,a+n-k)
lambda <- pbinom(y,n,p) / (pbinom(y,n,p) + m2)
lambda










###############################################################
#
#
#                        第 4 章
#
#
###############################################################


library(LearnBayes)
######################################################
# Section 4.2 パラメータが二つとも未知の正規データ
#    Normal Data with Both Parameters Unknown
######################################################


data(marathontimes)
attach(marathontimes)

d <- mycontour(normchi2post, c(220, 330, 500, 9000), time,
    xlab="mean",ylab="variance")




S <- sum((time - mean(time))^2) 
n <- length(time)
sigma2 <- S/rchisq(1000, n - 1)
mu <- rnorm(1000, mean = mean(time), sd = sqrt(sigma2)/sqrt(n))


points(mu, sigma2)



quantile(mu, c(0.025, 0.975))






###################################################
# Section 4.3 多項モデル
#    A Multinomial Model
###################################################



alpha <- c(728, 584, 138)
theta <- rdirichlet(1000, alpha)

hist(theta[, 1] - theta[, 2], main="")


data(election.2008)
attach(election.2008)

prob.Obama <- function(j)
 {
 p <- rdirichlet(5000,
   500 * c(M.pct[j], O.pct[j], 100 - M.pct[j] - O.pct[j])/100+1)
 mean(p[,2]>p[,1])
 }

Obama.win.probs <- sapply(1:51,prob.Obama)

sim.election <- function()
 {
 winner <- rbinom(51,1,Obama.win.probs)  
 sum(EV * winner)         
 }

sim.EV <- replicate(1000, sim.election())

 ## hist(sim.EV,min(sim.EV):max(sim.EV),col="blue")
 ## abline(v=365,lwd=3)  # Obama received 365 votes
 ## text(375,30,"Actual \n Obama \n total")


 hist(sim.EV,min(sim.EV):max(sim.EV),col="blue")
 abline(v=365,lwd=3)  # Obama received 365 votes
 text(375,30,"オバマの \n 実際の \n 得票数")



###################################################
# Section 4.4 生物検定
#     A Bioassay Experiment
###################################################

x <- c(-0.86, -0.3, -0.05, 0.73)
n <- c(5, 5, 5, 5)
y <- c(0, 1, 3, 5)
dat <- cbind(x, n, y)

response <- cbind(y, n - y)
results <- glm(response ~ x, family = binomial)
summary(results)


(a1.b1 <- beta.select(list(p = .5, x = .2), list(p = .9,x = .5)))

(a2.b2 <- beta.select(list(p = .5, x= .8), list(p = .9, x = .98)))



plot(c(-1,1), c(0,1), type = "n", xlab = "Dose",
     ylab = "Prob(death)")
lines(-0.7*c(1,1), qbeta(c(.25,.75), a1.b1[1], a1.b1[2]), lwd = 4)
lines(0.6*c(1,1), qbeta(c(.25,.75), a2.b2[1], a2.b2[2]), lwd = 4)
points(c(-0.7,0.6), qbeta(.5, c(a1.b1[1], a2.b2[1]), c(a1.b1[2], a2.b2[2])),
    pch=19,cex=2)
text(-0.3,.2, "Beta(1.12, 3.56)")
text(.2,.8, "Beta(2.10, 0.74)")
response <- rbind(a1.b1, a2.b2)
x <- c(-0.7,0.6)
fit <- glm(response ~ x, family = binomial)
curve(exp(fit$coef[1] + fit$coef[2] * x)/
     (1 + exp(fit$coef[1] + fit$coef[2] * x )), add = T)



prior <- rbind(c(-0.7, 4.68, 1.12),
     c(0.6, 2.84, 2.10))
data.new <- rbind(dat, prior)

mycontour(logisticpost, c(-3, 3, -1, 11), data.new,
  xlab="beta0", ylab="beta1")

s <- simcontour(logisticpost,c(-3,3,-1,11),data.new,1000)
points(s)

plot(density(s$y), xlab = "beta1")


theta <- -s$x/s$y
hist(theta, xlab = "LD-50", breaks = 20)



###########################################
# Section 4.5 二つの割合を比較する
#     Comparing Two Proportions
###########################################

sigma <- c(2,1,.5,.25)
 plo <- .0001;phi <- .9999
 par(mfrow = c(2,2))
 for (i in 1:4)
   mycontour(howardprior,c(plo,phi,plo,phi), c(1, 1, 1, 1, sigma[i]),
      main = paste("sigma=", as.character(sigma[i])),
      xlab = "p1", ylab = "p2")

sigma <- c(2, 1, .5, .25)
par(mfrow = c(2, 2))
for (i in 1:4)
{
 mycontour(howardprior,c(plo,phi,plo,phi),
   c(1+3,1+15,1+7,1+5,sigma[i]),
   main = paste("sigma=", as.character(sigma[i])),
   xlab = "p1", ylab = "p2")
 lines(c(0,1), c(0,1))
 }


s <- simcontour(howardprior,c(plo,phi,plo,phi),
   c(1+3,1+15,1+7,1+5,2),1000)
sum(s$x>s$y)/1000




###############################################################
#
#
#                        第 5 章
#
#
###############################################################



# 5.3 R で問題を設定する
mylogposterior <- function(theta, data)
{
n <- length(data)
mu <- theta[1]; sigma <- exp(theta[2])
logf <- function(y, mu, sigma)
dnorm(y, mean = mu, sd = sigma, log = TRUE)
val <- dnorm(mu, mean = 10, sd = 20, log = TRUE) +
sum(logf(data, mu, sigma))
return(val)
}

#####################################################
# Section 5.4 A 過分散に対するベータ・二項モデル
#    Beta-Binomial Model for Overdispersion
#####################################################


betabinexch0=function (theta, data) 
{
    eta = theta[1]
    K = theta[2]
    y = data[, 1]
    n = data[, 2]
    N = length(y)
    logf = function(y, n, K, eta) lbeta(K * eta + y, K * (1 - 
        eta) + n - y) - lbeta(K * eta, K * (1 - eta))
    val = sum(logf(y, n, K, eta))
    val = val - 2 * log(1 + K) - log(eta) - log(1 - eta)
    return(val)
}

data(cancermortality)

mycontour(betabinexch0,c(.0001,.003,1,20000),cancermortality,
    xlab="eta",ylab="K")


betabinexch=function (theta, data) 
{
    eta = exp(theta[1])/(1 + exp(theta[1]))
    K = exp(theta[2])
    y = data[, 1]
    n = data[, 2]
    N = length(y)
    logf = function(y, n, K, eta) lbeta(K * eta + y, K * (1 - 
        eta) + n - y) - lbeta(K * eta, K * (1 - eta))
    val = sum(logf(y, n, K, eta))
    val = val + theta[2] - 2 * log(1 + exp(theta[2]))
    return(val)
}

mycontour(betabinexch, c(-8,-4.5,3,16.5), cancermortality,
   xlab = "logit eta", ylab = "log K")




######################################################
# Section 5.6  事例
#   The Example
######################################################


fit <- laplace(betabinexch, c(-7,6), cancermortality)
fit


npar <- list(m = fit$mode, v = fit$var)
mycontour(lbinorm,c(-8,-4.5,3,16.5), npar,
     xlab = "logit eta", ylab = "log K")



se <- sqrt(diag(fit$var))

fit$mode-1.645*se

fit$mode+1.645*se



#########################################################
# Section 5.7 積分計算のためのモンテカルロ法
#   Monte Carlo Method for Computing Integrals
#########################################################


p <- rbeta(1000, 14.26, 23.19)
est <- mean(p^2)
se <- sd(p^2)/sqrt(1000)
c(est,se)


#########################################################
# Section 5.8 棄却サンプリング
#   Rejection Sampling
#########################################################

data(cancermortality)
fit=laplace(betabinexch,c(-7,6),cancermortality)

betabinT <- function(theta,datapar)
{
data <- datapar$data
tpar <- datapar$par
d <- betabinexch(theta,data) - dmt(theta, mean = c(tpar$m),
  S <- tpar$var, df = tpar$df, log = TRUE)
return(d)
}

tpar <- list(m = fit$mode, var = 2*fit$var, df = 4)
datapar <- list(data = cancermortality, par = tpar)

start <- c(-6.9,12.4)
fit1 <- laplace(betabinT, start, datapar)
fit1$mode


betabinT(fit1$mode,datapar)

theta <- rejectsampling(betabinexch,tpar,-569.2813,10000,
   cancermortality)
dim(theta)

mycontour(betabinexch, c(-8, -4.5, 3, 16.5), cancermortality,
 xlab = "logit eta", ylab = "log K")
points(theta[,1], theta[,2])

#############################################
# Section 5.9 重点サンプリング
#   Importance Sampling
#############################################

data(cancermortality)

fit <- laplace(betabinexch,c(-7,6),cancermortality)



betabinexch.cond <- function (log.K, data) 
{
    eta <- exp(-6.818793)/(1 + exp(-6.818793))
    K <- exp(log.K)
    y <- data[, 1]; n = data[, 2]; N = length(y)
    logf <- 0*log.K 
    for (j in 1:length(y))
      logf <- logf + lbeta(K * eta + y[j], K * (1 - 
        eta) + n[j] - y[j]) - lbeta(K * eta, K * (1 - eta))
    val <- logf + log.K - 2 * log(1 + K)
    return(exp(val-max(val)))
}


 Int <- integrate(betabinexch.cond, 2, 16, cancermortality)
 par(mfrow = c(2, 2))
 curve(betabinexch.cond(x, cancermortality)/Int$value, from = 3, to = 16,
  ylab = "Density", xlab = "log K", lwd = 3, main = "Densities")
 curve(dnorm(x, 8, 2), add = TRUE)
 legend("topright", legend=c("Exact", "Normal"), lwd = c(3, 1))
 curve(betabinexch.cond(x, cancermortality)/Int$value/
        dnorm(x, 8, 2), from = 3, to = 16, ylab = "Weight", xlab= "log K",
        main = "Weight = g/p")

curve(betabinexch.cond(x, cancermortality)/Int$value, from = 3, to = 16,
   ylab = "Density", xlab = "log K", lwd = 3, main = "Densities")
curve(1/2 * dt(x - 8, df = 2), add = TRUE)
legend("topright", legend = c("Exact", "T(2)"), lwd = c(3, 1))
curve(betabinexch.cond(x, cancermortality)/Int$value/
        (1/2 * dt(x - 8, df = 2)), from = 3, to = 16,
      ylab = "Weight", xlab = "log K",  main = "Weight = g/p")

#

tpar <- list(m = fit$mode, var = 2 * fit$var, df = 4)
 myfunc <- function(theta)
   return(theta[2])
s <- impsampling(betabinexch, tpar, myfunc, 10000, cancermortality)
cbind(s$est, s$se)



##############################################
# Section 5.10 サンプリング重点リサンプリング(SIR)
#  Sampling Importance Resampling
##############################################

library(LearnBayes)
data(cancermortality)

fit <- laplace(betabinexch, c(-7,6), cancermortality)

tpar <- list(m = fit$mode, var = 2 * fit$var, df = 4)

theta.s <- sir(betabinexch, tpar, 10000, cancermortality)

S <- bayes.influence(theta.s, cancermortality)

plot(c(0,0,0), S$summary, type = "b", lwd = 3, xlim = c(-1,21),
  ylim = c(5,11), xlab = "Observation removed", ylab = "log K")

for (i in 1:20)
  lines(c(i,i,i), S$summary.obs[i,], type = "b")




###############################################################
#
#
#                        第 6 章
#
#
###############################################################

library(LearnBayes)

####################################################
# Section 6.2 離散的マルコフ連鎖
#   Introduction to Discrete Markov Chains
####################################################

 P <- matrix(c(.5,.5,0,0,0,0,.25,.5,.25,0,0,0,0,.25,.5,.25,0,0,
           0,0,.25,.5,.25,0,0,0,0,.25,.5,.25,0,0,0,0,.5,.5),
           nrow=6,ncol=6,byrow=TRUE)
 P

 s <- array(0,c(50000,1))

 s[1] <- 3
 for (j in 2:50000)
   s[j] <- sample(1:6,size=1,prob=P[s[j-1],])

 m <- c(500,2000,8000,50000)
 for (i in 1:4)
   print(table(s[1:m[i]])/m[i])

 w <- matrix(c(.1,.2,.2,.2,.2,.1),nrow=1,ncol=6)
 w%*%P


##################################################################
# Section 6.7 グループ化データからの正規母集団を検討する
#  Learning about a Normal Population from Grouped Data
##################################################################

library(LearnBayes)


groupeddatapost <- function(theta,data) 
{
    dj <- function(f, int.lo, int.hi, mu, sigma) 
        f * log(pnorm(int.hi, mu, sigma) - 
        pnorm(int.lo, mu, sigma))
    mu <- theta[1]
    sigma <- exp(theta[2])
    sum(dj(data$f, data$int.lo, data$int.hi, mu, sigma))
}


d <- list(int.lo = c(-Inf, seq(66, 74, by = 2)),
        int.hi = c(seq(66, 74, by = 2), Inf),
        f=c(14, 30, 49, 70, 33, 15))

y <- c(rep(65,14), rep(67,30), rep(69,49), rep(71,70),
       rep(73,33), rep(75,15))
mean(y)

log(sd(y))

start <- c(70,1)
fit <- laplace(groupeddatapost, start, d)
fit

modal.sds <- sqrt(diag(fit$var))

proposal <- list(var = fit$var, scale = 2)
fit2 <- rwmetrop(groupeddatapost, proposal, start, 10000, d)

fit2$accept

post.means <- apply(fit2$par,2,mean)
post.sds <- apply(fit2$par,2,sd)

cbind(c(fit$mode),modal.sds)
cbind(post.means,post.sds)

mycontour(groupeddatapost,c(69, 71, .6, 1.3), d,
    xlab = "mu", ylab = "log sigma")
points(fit2$par[5001:10000,1], fit2$par[5001:10000, 2])



##################################################
# Section 6.8 出力分析の事例
#  Example of Output Analysis
##################################################
 library(LearnBayes)

start <- c(65,1)
fit <- laplace(groupeddatapost, start, d)
fit

# modal.sds <- sqrt(diag(fit$var))
proposal <- list(var=fit$var, scale=0.2)

bayesfit <- rwmetrop(groupeddatapost, proposal, start, 10000, d)


# install.packages("coda")
library(coda)

dimnames(bayesfit$par)[[2]] = c("mu", "log sigma")
library(lattice)
xyplot(mcmc(bayesfit$par[-c(1:2000),]), col = "black")


bayesfit$accept

d <- list(int.lo = c(-Inf, seq(66,74, by = 2)),
        int.hi = c(seq(66,74,by = 2), Inf),
        f = c(14,30,49,70,33,15))

library(coda)
 
start <- c(70,1)
fit <- laplace(groupeddatapost,start,d)

start <- c(65,1)
proposal <- list(var=fit$var,scale=0.2)
bayesfit <- rwmetrop(groupeddatapost,proposal,start,10000,d)

dimnames(bayesfit$par)[[2]] <- c("mu","log sigma")
xyplot(mcmc(bayesfit$par[-c(1:2000),]), col = "black")
par(mfrow = c(2,1))
autocorr.plot(mcmc(bayesfit$par[-c(1:2000),]), auto.layout = FALSE)


summary(mcmc(bayesfit$par[-c(1:2000), ]))
batchSE(mcmc(bayesfit$par[-c(1:2000), ]), batchSize = 50)

start <- c(70,1)
proposal <- list(var = fit$var, scale = 2.0)
bayesfit <- rwmetrop(groupeddatapost, proposal, start, 10000, d)

dimnames(bayesfit$par)[[2]] <- c("mu", "log sigma")
sim.parameters <- mcmc(bayesfit$par[-c(1:2000),])
xyplot(mcmc(bayesfit$par[-c(1:2000),]), col = "black")

par(mfrow = c(2,1))
autocorr.plot(sim.parameters, auto.layout = FALSE)
summary(sim.parameters)
batchSE(sim.parameters, batchSize = 50)





###################################################
# Section 6.9 コーシー誤差によるデータのモデリング
#  Modeling Data with Cauchy Errors
###################################################

library(LearnBayes)

 data(darwin)
 attach(darwin)
 mean(difference)

 log(sd(difference))

 laplace(cauchyerrorpost, c(21.6,3.6), difference)

 laplace(cauchyerrorpost,.1 * c(21.6,3.6), difference)$mode
 
 c(24.7-4*sqrt(34.96),24.7 + 4*sqrt(34.96))
 c(2.77-4*sqrt(.138),2.77 + 4*sqrt(.138))

 mycontour(cauchyerrorpost,c(-10,60,1,4.5),difference,
   xlab = "mu", ylab = "log sigma")

 fitlaplace <- laplace(cauchyerrorpost,c(21.6,3.6), difference)
 mycontour(lbinorm, c(-10,60,1,4.5), list(m = fitlaplace$mode,
   v=fitlaplace$var), xlab = "mu", ylab = "log sigma")

 proposal <- list(var = fitlaplace$var, scale = 2.5)
 start <- c(20,3)
 m <- 1000
 s <- rwmetrop(cauchyerrorpost, proposal, start, m, difference)
 mycontour(cauchyerrorpost, c(-10,60,1,4.5), difference,
   xlab = "mu", ylab = "log sigma")
 points(s$par[,1], s$par[,2])

 fitgrid <- simcontour(cauchyerrorpost, c(-10,60,1,4.5) ,difference,
  50000)
 proposal <- list(var = fitlaplace$var, scale = 2.5)
 start <- c(20,3)
 fitrw <- rwmetrop(cauchyerrorpost, proposal, start, 50000,
   difference)
 proposal2 <- list(var = fitlaplace$var, mu = t(fitlaplace$mode))
 fitindep <- indepmetrop(cauchyerrorpost, proposal2, start, 50000,
  difference)
 fitgibbs <- gibbs(cauchyerrorpost, start, 50000, c(12,.75),
  difference)

 apply(fitrw$par,2,mean)

 apply(fitrw$par,2,sd)



#############################################################
# Section 6.10 スタンフォード心臓移植手術データの分析
#   Analysis of the Stanford Heart Transplant Data
#############################################################

library(LearnBayes)

data(stanfordheart)

head(stanfordheart)

transplantpost <- function (theta, data) 
{
    x <- data[, 1]
    y <- data[, 3]
    t <- data[, 2]
    d <- data[, 4]
    tau <- exp(theta[1])
    lambda <- exp(theta[2])
    p <- exp(theta[3])
    xnt <- x[t == 0]
    dnt <- d[t == 0]
    z <-  x[t == 1]
    y <- y[t == 1]
    dt <- d[t == 1]
    logf <- function(xnt, dnt, lambda, p) 
        (dnt == 0) * (p * log(lambda) + 
        log(p) - (p + 1) * log(lambda + xnt)) + (dnt == 1) * 
        p * log(lambda/(lambda + xnt))
    logg <- function(z, y, tau, lambda, p) 
        (dt == 0) * (p * log(lambda) + 
        log(p * tau) - (p + 1) * log(lambda + y + tau * z)) + 
        (dt == 1) * p * log(lambda/(lambda + y + tau * z))
    val <- sum(logf(xnt, dnt, lambda, p)) + 
          sum(logg(z, y, tau, lambda, p))
    val <- val + theta[1] + theta[2] + theta[3]
    return(val)
}


 start <- c(0,3,-1)
 laplacefit <- laplace(transplantpost, start, stanfordheart)
 laplacefit

 proposal <- list(var = laplacefit$var, scale = 2)
 s <- rwmetrop(transplantpost, proposal, start, 10000, stanfordheart)
 s$accept

 par(mfrow = c(2,2))
 tau <- exp(s$par[,1])
 plot(density(tau), main = "TAU")

 lambda <- exp(s$par[, 2])
 plot(density(lambda),main = "LAMBDA")

 p <- exp(s$par[,3])
 plot(density(p), main = "P")

 apply(exp(s$par), 2, quantile, c(.05,.5,.95))

 par(mfrow = c(1,1))
  t=seq(1, 240)
 p5 <- 0*t; p50 <- 0*t; p95 <- 0*t

 for (j in 1:240)
 {
   S <- (lambda/(lambda+t[j]))^p
   q <- quantile(S,c(.05,.5,.95))
   p5[j] <- q[1]; p50[j] <- q[2]; p95[j] <- q[3]}
 plot(t,p50,type="l",ylim = c(0,1),
      ylab = "Prob(Survival)",xlab="time")
lines(t,p5,lty=2)
lines(t,p95,lty=2)






###############################################################
#
#
#                        第 7 章
#
#
###############################################################

library(LearnBayes)




#####################################################
#  Section 7.3 個々の推定と組み合わせ推定
#    Individual and Combined Estimates
#####################################################

library(LearnBayes)
data(hearttransplants)
attach(hearttransplants)

plot(log(e), y/e, xlim = c(6,9.7), xlab = "log(e)", ylab = "y/e")
text(log(e), y/e, labels = as.character(y), pos = 4)

#####################################################
#  Section 7.4 Equal Mortality Rates?
#  死亡率は等しいか？
#####################################################


library(LearnBayes)
data(hearttransplants)
attach(hearttransplants)

sum(y)
sum(e)

 lambda <- rgamma(1000, shape = 277, rate = 294681)
 ys94 <- rpois(1000, e[94] * lambda)

 hist(ys94,breaks=seq(0.5,max(ys94) + 0.5))
 lines(c(y[94],y[94]),c(0,120), lwd = 3)

lambda <- rgamma(1000, shape = 277, rate = 294681)

prob.out <- function(i)
{
   ysi <- rpois(1000, e[i] * lambda)
   pleft <- sum(ysi <= y[i])/1000
   pright <- sum(ysi >= y[i])/1000
   min(pleft,pright)
 }
pout <- sapply(1:94, prob.out)

sum(pout < .1)
plot(log(e), pout, ylab = "Prob(extreme)" )

#####################################################
#  Section 7.5 交換可能性を事前の確信とするモデリング
#  Modeling a Prior Belief of Exchangeability
#####################################################


library(LearnBayes)

pgexchprior <- function(lambda,pars)
{
alpha <- pars[1]; a <- pars[2]; b <- pars[3]
(alpha - 1) * log(prod(lambda)) - (2 * alpha+a) * log(alpha * sum(lambda) + b)
}

alpha <- c(5,20,80,400)
par(mfrow = c(2, 2))


for (j in 1:4)
    mycontour(pgexchprior, c(.001,5,.001,5), c(alpha[j],10,10),
      main=paste("ALPHA = ",alpha[j]), xlab = "LAMBDA 1", ylab = "LAMBDA 2")



#####################################################
#  Section 7.7 事後分布からのシミュレーション
#    Simulating from the Posterior
#####################################################


library(LearnBayes)


data(hearttransplants)
attach(hearttransplants)

datapar <- list(data = hearttransplants, z0 = 0.53)
start <- c(2, -7)
fit <- laplace(poissgamexch, start, datapar)
fit

par(mfrow = c(1, 1))
mycontour(poissgamexch, c(0, 8, -7.3, -6.6), datapar,
   xlab = "log alpha", ylab = "log mu")

start <- c(4, -7)
fitgibbs <- gibbs(poissgamexch, start, 1000, c(1,.15), datapar)
fitgibbs$accept

mycontour(poissgamexch, c(0, 8, -7.3, -6.6), datapar,
     xlab = "log alpha", ylab = "log mu")



points(fitgibbs$par[, 1], fitgibbs$par[, 2])

plot(density(fitgibbs$par[, 1], bw = 0.2))

alpha <- exp(fitgibbs$par[, 1])
mu <- exp(fitgibbs$par[, 2])
lam1 <- rgamma(1000, y[1] + alpha, e[1] + alpha/mu)

alpha <- exp(fitgibbs$par[, 1])
mu <- exp(fitgibbs$par[, 2])


plot(log(e), y/e, pch = as.character(y))


 for (i in 1:94) {
     lami <- rgamma(1000, y[i] + alpha, e[i] + alpha/mu)
     probint <- quantile(lami, c(0.05, 0.95))
     lines(log(e[i]) * c(1, 1), probint)
 }


#####################################################
#  Section 7.8  事後推論
#    Posterior Inferences
#####################################################

 

shrink <- function(i) mean(alpha/(alpha + e[i] * mu))
shrinkage <- sapply(1:94, shrink)

plot(log(e), shrinkage)

mrate <- function(i) mean(rgamma(1000, y[i] + alpha, e[i] + alpha/mu))
hospital <- 1:94
meanrate <- sapply(hospital,mrate)
hospital[meanrate==min(meanrate)]


sim.lambda <- function(i) rgamma(1000, y[i] + alpha, e[i] + alpha/mu)
LAM <- sapply(1:94,sim.lambda)

compare.rates <- function(x) {
  nc <- NCOL(x)
  ij <- as.matrix(expand.grid(1:nc, 1:nc))
  m <- as.matrix(x[,ij[,1]] > x[,ij[,2]]) 
  matrix(colMeans(m), nc, nc, byrow = TRUE)
}

better <- compare.rates(LAM)

better[1:24,85]



#####################################################
#  Section 7.9   ベイズ感度分析
#    Bayesian Sensitivity Analysis
#####################################################


library(LearnBayes)
data(hearttransplants)
attach(hearttransplants)

datapar <- list(data = hearttransplants, z0 = 0.53)

start <- c(4, -7)
fitgibbs <- gibbs(poissgamexch, start, 1000, c(1,.15), datapar)


sir.old.new <- function(theta, prior, prior.new)
{
 log.g <- log(prior(theta))
 log.g.new <- log(prior.new(theta))
 wt <- exp(log.g.new - log.g - max(log.g.new - log.g))
 probs <- wt/sum(wt)
 n <- length(probs)
 indices <- sample(1:n, size = n, prob = probs, replace = TRUE)
 theta[indices]
}

prior <- function(theta)
  0.53 * exp(theta) / (exp(theta) + 0.53)^2
prior.new <- function(theta)
  5 * exp(theta) / (exp(theta) + 5)^2

log.alpha <- fitgibbs$par[, 1]
log.alpha.new <- sir.old.new(log.alpha, prior, prior.new)


library(lattice)
draw.graph <- function()
{
LOG.ALPHA <- data.frame("prior",log.alpha)
names(LOG.ALPHA) <- c("Prior","log.alpha")
LOG.ALPHA.NEW <- data.frame("new.prior", log.alpha.new)
names(LOG.ALPHA.NEW) <- c("Prior","log.alpha")
D <- densityplot(~ log.alpha, group = Prior,
  data = rbind(LOG.ALPHA, LOG.ALPHA.NEW),
   plot.points = FALSE, main = "Original Prior and Posterior (solid), 
   New Prior and Posterior (dashed)",
    lwd=4,adjust=2,lty=c(1,2), xlab = "log alpha", xlim = c(-3,5), col = "black")
update(D, panel=function(...){
    panel.curve(prior(x), lty = 1,lwd = 2, col = "black")
    panel.curve(prior.new(x), lty = 2, lwd = 2, col = "black")
    panel.densityplot(...)
})}

draw.graph()


prob.int.rate <- function(i,log.alpha)
{
     lami <- rgamma(1000, y[i] + exp(log.alpha), e[i] + exp(log.alpha)/mu)
     quantile(lami, c(0.05, 0.95))
}

ind <- c(1,25,57,89)

ci <- sapply(ind,prob.int.rate,log.alpha)
matplot(matrix(log(e[ind]), 2, length(ind), byrow = T),
        ci, type = "l",    lwd = 4, lty = 1,
        xlim = c(6,9.7), ylim = c(0,.002),
        ylab = "True Rate", xlab = "log(e)")

ci <- sapply(ind, prob.int.rate, log.alpha.new)
matplot(matrix(log(e[ind]), 2, length(ind), byrow = T) + .05,
        ci,type = "l", lwd = 4, lty = 3,  add = TRUE)



#####################################################
#  Section 7. 10 事後予測モデルの検証
#    Posterior Predictive Model Checking 
#####################################################

library(LearnBayes)
data(hearttransplants)
attach(hearttransplants)

datapar <- list(data = hearttransplants, z0 = 0.53)

start <- c(4, -7)
fitgibbs <- gibbs(poissgamexch, start, 1000, c(1,.15), datapar)

alpha <- exp(fitgibbs$par[, 1])
mu <- exp(fitgibbs$par[, 2])

lam94 <- rgamma(1000, y[94] + alpha, e[94] + alpha/mu)

ys94 <- rpois(1000, e[94] * lam94)

hist(ys94, breaks = seq(-0.5, max(ys94) + 0.5))
lines(y[94] * c(1,1), c(0,100), lwd = 3)




prob.out <- function(i)
{
   lami <- rgamma(1000,y[i] + alpha, e[i] + alpha/mu)
   ysi <- rpois(1000, e[i] * lami)
   pleft <- sum(ysi <= y[i])/1000
   pright <- sum(ysi >= y[i])/1000
   min(pleft, pright)
 }


pout.exchange <- sapply(1:94,prob.out)

plot(pout, pout.exchange, xlab = "P(extreme), equal means",
     ylab  = "P(extreme), exchangeable") # pout 7.4 節
abline(0,1)






###############################################################
#
#
#                        第 8 章
#
#
###############################################################


###############################################
# Section 8.3  正規平均の片側検定
#  A One-Sided Test of a Normal Mean
###############################################

library(LearnBayes)

 pmean <- 170; pvar <- 25
 probH <- pnorm(175,pmean,sqrt(pvar))
 probA <- 1 - probH
 prior.odds <- probH/probA
 prior.odds

 weights <- c(182, 172, 173, 176, 176, 180, 173, 174, 179, 175)

# weights *  453.59237 / 1000
 xbar <- mean(weights)
 sigma2 <- 3^2/length(weights)

 post.precision <- 1/sigma2+1/pvar
 post.var <- 1/post.precision

 post.mean <- (xbar/sigma2 + pmean/pvar)/post.precision
 c(post.mean, sqrt(post.var))0

 post.odds <- pnorm(175, post.mean, sqrt(post.var)) /
  (1 - pnorm(175, post.mean, sqrt(post.var)))
 post.odds

 BF <- post.odds/prior.odds
 BF

 postH <- probH * BF/(probH * BF + probA)
 postH

 z <- sqrt(length(weights))*(mean(weights)-175)/3
 1-pnorm(z)

 weights <- c(182, 172, 173, 176, 176, 180, 173, 174, 179, 175)
 data <- c(mean(weights), length(weights),3)
 prior.par <- c(170, 1000)
 mnormt.onesided(175, prior.par, data)


#################################################
# Section 8.4  正規平均の両側検定
#   A Two-Sided Test of a Normal Mean
#################################################

library(LearnBayes)

 weights <- c(182, 172, 173, 176, 176, 180, 173, 174, 179, 175)
 data <- c(mean(weights), length(weights), 3)
 t <- c(.5,1,2,4,8)
 mnormt.twosided(170, .5, t, data)


#################################################
# Section 8.6 サッカーのゴール数のモデル
#   Models for Soccer Goals
#################################################

library(LearnBayes)
data(soccergoals)
 
y <- soccergoals$goals
 
 #write the likelihood function via the gamma distribution
 
 
lik_pois<- function(data, theta){
   n <- length(data)
   lambda <- exp(theta)
   dgamma(lambda, shape =sum(data)+1, scale=1/n)
 }
 
prior_gamma <- function(par, theta){
   lambda <- exp(theta)
   dgamma(lambda, par[1], rate=par[2])*lambda  
 }
 
prior_norm <- function(npar, theta){
   lambda=exp(theta)  
   (dnorm(theta, npar[1], npar[2]))
   
 }
 
lik_pois<- function(data, theta){
   n <- length(data)
   lambda <- exp(theta)
   dgamma(lambda, shape =sum(data)+1, scale=1/n)
 }
 
prior_gamma <- function(par, theta){
   lambda <- exp(theta)
   dgamma(lambda, par[1], rate=par[2])*lambda  
 }
 
prior_norm <- function(npar, theta){
   lambda=exp(theta)  
   (dnorm(theta, npar[1], npar[2]))
   
 }

par(mfrow = c(2, 1))
curve(lik_pois(theta = x, data = y), xlim = c(-1, 4), xlab = expression(theta), ylab = "density", lwd = 1 )
curve(prior_gamma(theta = x, par = c(4.57, 1.43)), lty = 2, col = 2, add = TRUE, lwd = 2)
curve(prior_norm(theta = x, npar = c(1, .5)), lty = 3, col = 3, add = TRUE, lwd = 3)
legend(3.0, 1.8, c("Lik", "prior1", "prior2"), lty = 1:3, col = 1:3, lwd = 1:3, cex = 0.9)

curve(lik_pois(theta = x, data = y), xlim=c(-1,4), xlab=expression(theta), ylab = "density", lwd = 1 )
curve(prior_norm(theta = x, npar = c(2, .5)), xlim = c(-1,4), lty =4, col = 2, add = TRUE, 
      lwd = 2)
curve(prior_norm(theta = x, npar = c(1, 2)), lty = 5, col = 3, add = TRUE, lwd = 3)
legend(3.0, 1.8, c("Lik", "prior3", "prior4" ), lty = 1:3, col = 1:3, lwd = 1:3, cex = 0.9)

dev.off()


datapar <- list(data = goals, par = c(4.57,1.43))
fit1 <- laplace(logpoissgamma, .5, datapar)
datapar <- list(data = goals, par = c(1,.5))
fit2 <- laplace(logpoissnormal, .5, datapar)
datapar <- list(data = goals, par = c(2,.5))
fit3 <- laplace(logpoissnormal, .5, datapar)
datapar <- list(data = goals, par = c(1,2))
fit4 <- laplace(logpoissnormal, .5, datapar)

postmode <- c(fit1$mode, fit2$mode, fit3$mode, fit4$mode)
postsd <- sqrt(c(fit1$var, fit2$var, fit3$var, fit4$var))
logmarg <- c(fit1$int, fit2$int, fit3$int, fit4$int)
cbind(postmode, postsd, logmarg)

 
BF_matrix <- matrix(1, 4,4)
 for (i in 1:3){
   for (j in 2:4){
     BF_matrix[i,j]<- exp(logmarg[i]-logmarg[j])
     BF_matrix[j,i]=(1/BF_matrix[i,j]) 
   }
}
 
round_bf <- round(BF_matrix,3)
round_bf


###################################################
# Section 8.7  野球選手の打率には本当にむらがあるのか？
#   Is a Baseball Hitter Really Streaky?
###################################################

library(LearnBayes)

 data(jeter2004)
 attach(jeter2004)
 data <- cbind(H,AB)
 data1 <- regroup(data,5)

 log.marg <- function(logK) 
     laplace(bfexch,0,list(data = data1, K = exp(logK)))$int
 
 log.K <- seq(2,6)
 K <- exp(log.K)
 log.BF <- sapply(log.K,log.marg)
 BF <- exp(log.BF)
 round(data.frame(log.K, K, log.BF, BF), 2)



###################################################################
# Section 8.8         2 元分割表の独立性の検定
#   A Test of Independence in a Two-Way Contingency Table
###################################################################

library(LearnBayes)

 data <- matrix(c(11,9,68,23,3,5),c(2,3))
 data

 chisq.test(data)

 a <- matrix(rep(1,6),c(2,3))
 a

 ctable(data,a)

  log.K <- seq(2,7)
  compute.log.BF <- function(log.K)
     log(bfindep(data,exp(log.K),100000)$bf)
  log.BF <- sapply(log.K,compute.log.BF)
  BF <- exp(log.BF) 
round(data.frame(log.K,log.BF,BF),2)








###############################################################
#
#
#                        第 9 章
#
#
###############################################################


################################
# Section 9.2.6   事例
#  An Example
################################

library(LearnBayes)

 data(birdextinct)
 attach(birdextinct)


 logtime <- log(time)


 plot(nesting, logtime)
 out <- (logtime > 3)
 text(nesting[out], logtime[out], label = species[out], pos = 2)	
 plot(jitter(size), logtime, xaxp = c(0,1,1))
 plot(jitter(status), logtime, xaxp = c(0,1,1))

##### Least-squares fit

 fit <- lm(logtime ~ nesting + size + status,
           data = birdextinct, x = TRUE, y = TRUE)
 summary(fit)

##### Sampling from posterior

 theta.sample <- blinreg(fit$y,fit$x,5000)

 par(mfrow=c(2,2))
 hist(theta.sample$beta[,2], main = "NESTING",
  xlab=expression(beta[1]))
 hist(theta.sample$beta[,3], main = "SIZE",
  xlab=expression(beta[2]))
 hist(theta.sample$beta[,4], main = "STATUS",
  xlab=expression(beta[3]))
 hist(theta.sample$sigma, main = "ERROR SD",
  xlab=expression(sigma))

 apply(theta.sample$beta, 2, quantile,c(.05,.5,.95))

 quantile(theta.sample$sigma, c(.05,.5,.95))

###### Estimating mean extinction times

 cov1 <- c(1,4,0,0)
 cov2 <- c(1,4,1,0)
 cov3 <- c(1,4,0,1)
 cov4 <- c(1,4,1,1)
 X1 <- rbind(cov1, cov2, cov3, cov4)
 mean.draws <- blinregexpected(X1, theta.sample)

 c.labels <- c("A","B","C","D")
 par(mfrow=c(2,2))
 for (j in 1:4)
   hist(mean.draws[,j],
      main = paste("Covariate set", c.labels[j]), xlab = "log TIME")
 
######## Predicting extinction times

 cov1 <- c(1,4,0,0)
 cov2 <- c(1,4,1,0)
 cov3 <- c(1,4,0,1)
 cov4 <- c(1,4,1,1)
 X1 <- rbind(cov1, cov2, cov3, cov4)
 pred.draws <- blinregpred(X1, theta.sample)

 c.labels <- c("A","B","C","D")
 par(mfrow = c(2,2))
 for (j in 1:4)
   hist(pred.draws[,j],
      main = paste("Covariate set", c.labels[j]), xlab = "log TIME")

######### Model checking via posterior predictive distribution

 pred.draws <- blinregpred(fit$x, theta.sample)
 pred.sum <- apply(pred.draws, 2, quantile,c(.05,.95))
 par(mfrow = c(1,1))
 ind <- 1:length(logtime)
 matplot(rbind(ind,ind), pred.sum, type = "l", lty = 1, col = 1,
  xlab = "INDEX",ylab = "log TIME")
 points(ind, logtime, pch = 19)
 out <- (logtime > pred.sum[2,])
 text(ind[out], logtime[out], label = species[out], pos = 4)

######### Model checking via bayes residuals

 prob.out <- bayesresiduals(fit, theta.sample, 2)
 par(mfrow = c(1,1))
 plot(nesting, prob.out)
 out <- (prob.out > 0.35)
 text(nesting[out], prob.out[out], label = species[out], pos = 4)	






##############################################
# Section 9.3  Zellner の事前分布によるモデル選択
#   Modeling Using Zellner's g Prior
##############################################

library(LearnBayes)

# illustrating the role of the parameter c

data(puffin)
X <- cbind(1, puffin$Distance - mean(puffin$Distance))
c.prior <- c(0.1,0.5,5,2)
fit <- vector("list",4)
for (j in 1:4)
{
  prior <- list(b0 = c(8,0), c0 = c.prior[j])
  fit[[j]] <- blinreg(puffin$Nest, X, 1000, prior)
}
BETA <- NULL
for (j in 1:4)
  {
  s <- data.frame(Prior = paste("c =", as.character(c.prior[j])),
         beta0 = fit[[j]]$beta[,1], beta1 = fit[[j]]$beta[,2])
  BETA <- rbind(BETA, s)
  }


library(lattice)


with(BETA,xyplot(beta1 ~ beta0 | Prior,
                 type = c("p","g"), col = "black"))

# model selection

data <- list(y = puffin$Nest, X = cbind(1, puffin$Grass, puffin$Soil))
prior <- list(b0 = c(0,0,0), c0 = 100)
beta.start <- with(puffin, lm(Nest ~ Grass+Soil)$coef)
laplace(reg.gprior.post, c(beta.start,0), list(data = data, prior = prior))$int

X <- puffin[,-1]; y <- puffin$Nest; c <- 100
bayes.model.selection(y, X, c, constant = FALSE)



##############################################
# Section 9.4 生存モデル
#  Survival Modeling
##############################################

 library(LearnBayes)

 data(chemotherapy)
 attach(chemotherapy)
 library(survival)
 survreg(Surv(time, status) ~ factor(treat) + age,
         dist = "weibull")

 start <- c(-.5,9,.5,-.05)
 d <- cbind(time, status, treat-1, age)
 fit <- laplace(weibullregpost, start, d)
 fit

 proposal <- list(var = fit$var,scale = 1.5)
 bayesfit <- rwmetrop(weibullregpost, proposal, fit$mode, 10000, d)
 bayesfit$accept

 par(mfrow=c(2,2))
 sigma <- exp(bayesfit$par[,1])
 mu <- bayesfit$par[,2] # ?
 beta1 <- bayesfit$par[,3]
 beta2 <- bayesfit$par[,4]
 hist(beta1, xlab = "treatment", main = "")
 hist(beta2, xlab = "age", main = "")
 hist(sigma, xlab = "sigma", main = "")






###############################################################
#
#
#                        第 10 章
#
#
###############################################################


###############################
# Section 10.2 頑健なモデル
#    Robust Modeling
###############################

library(LearnBayes)

 data(darwin)
 attach(darwin)
 fit <- robustt(difference, 4, 10000)

 plot(density(fit$mu), xlab = "mu")

 mean.lambda <- apply(fit$lam, 2, mean)
 lam5 <- apply(fit$lam,2,quantile, .05)
 lam95 <- apply(fit$lam,2,quantile, .95)
 plot(difference, mean.lambda, lwd = 2, ylim = c(0, 3), ylab = "Lambda")
 for (i in 1:length(difference))
   lines(c(1,1) * difference[i], c(lam5[i], lam95[i]))
 points(difference, 0 * difference - .05, pch = 19, cex = 2)



#############################################################
# Section 10.3 プロビットリンクによる2 値反応の回帰
#  Binary Response Regression with a Probit Link
#############################################################

#################################################
# Section 10.3.1. 欠損データとギブスサンプリング
#  Missing data and Gibbs sampling
#################################################

library(LearnBayes)

 data(donner)
 attach(donner)
 X <- cbind(1,age,male)

 fit <- glm(survival ~ X-1, family = binomial(link = probit))
 summary(fit)

 m <- 10000
 fit <- bayes.probit(survival, X, m)

 apply(fit$beta, 2, mean)

 apply(fit$beta, 2, sd)

 a <- seq(15,65)
 X1 <- cbind(1,a,1)
 p.male <- bprobit.probs(X1, fit$beta)

 plot(a, apply(p.male, 2, quantile, .5),
      type = "l", ylim = c(0,1),
      xlab = "age", ylab = "Probability of Survival")
 lines(a,apply(p.male, 2, quantile, .05), lty = 2)
 lines(a,apply(p.male, 2, quantile, .95), lty = 2)


###################################################
# Section 10.3.2  正則事前分布とモデル選択
#  Proper priors and model selection
###################################################

library(LearnBayes)
data(donner)
y <- donner$survival
X <- cbind(1,donner$age, donner$male)

beta0 <- c(0,0,0); c0 <- 100
P0 <- t(X)%*%X/c0

bayes.probit(y, X, 1000, list(beta = beta0, P = P0))$log.marg

bayes.probit(y,X[,-2],1000,
   list(beta=beta0[-2],P=P0[-2,-2]))$log.marg

bayes.probit(y,X[,-3],1000,
   list(beta=beta0[-3],P=P0[-3,-3]))$log.marg

bayes.probit(y,X[,-c(2,3)],1000,
   list(beta=beta0[-c(2,3)], P  = P0[-c(2,3),-c(2,3)]))$log.marg





###################################################
# Section 10.4 平均表の推定
#   Estimating a Table of Means
###################################################

library(LearnBayes)

 data(iowagpa)
 rlabels <- c("91-99", "81-90", "71-80", "61-70", "51-60", "41-50", 
     "31-40", "21-30")
 clabels <- c("16-18", "19-21", "22-24", "25-27", "28-30")
 gpa <- matrix(iowagpa[, 1], nrow = 8, ncol = 5, byrow = T)
 dimnames(gpa) <- list(HSR = rlabels, ACTC = clabels)
 gpa

 samplesizes <- matrix(iowagpa[, 2], nrow = 8, ncol = 5, byrow = T)
 dimnames(samplesizes) <- list(HSR = rlabels, ACTC = clabels)
 samplesizes

 act <- seq(17, 29, by = 3)
 matplot(act, t(gpa), type = "l", lwd = 3, 
  xlim = c(17, 34), col=1:8, lty=1:8)
 legend(30, 3, lty = 1:8, lwd = 3, legend = c("HSR=9", "HSR=8", 
     "HSR=7", "HSR=6", "HSR=5", "HSR=4", "HSR=3", "HSR=2"), col=1:8)

 MU <- ordergibbs(iowagpa, 5000)

 postmeans <- apply(MU, 2, mean)
 postmeans <- matrix(postmeans, nrow = 8, ncol = 5)
 postmeans <- postmeans[seq(8,1,-1),]
 dimnames(postmeans) <- list(HSR=rlabels,ACTC=clabels)
 round(postmeans,2)

matplot(act, t(postmeans), type = "l", lty=1:8, lwd = 3,
        col = 1, xlim = c(17, 34))
 legend(30, 3, lty = 1:8, lwd = 2, legend = c("HSR=9", "HSR=8", 
     "HSR=7", "HSR=6", "HSR=5", "HSR=4", "HSR=3", "HSR=2"))

 postsds <- apply(MU, 2, sd)
 postsds <- matrix(postsds, nrow = 8, ncol = 5)
 postsds <- postsds[seq(8,1,-1),]
 dimnames(postsds) <- list(HSR=rlabels,ACTC=clabels)
 round(postsds,3)

 s <- .65
 se <- s/sqrt(samplesizes)
 round(postsds/se,2)

 FIT <- hiergibbs(iowagpa, 5000)

 par(mfrow = c(2,1))
 plot(density(FIT$beta[,2]), xlab = expression(beta[2]),
  main = "HIGH SCHOOL RANK")
 plot(density(FIT$beta[,3]), xlab = expression(beta[3]),
  main = "ACT SCORE")
 quantile(FIT$beta[,2],c(.025,.25,.5,.75,.975))

 quantile(FIT$beta[,3],c(.025,.25,.5,.75,.975))

 quantile(FIT$var,c(.025,.25,.5,.75,.975))

 posterior.means <- apply(FIT$mu, 2, mean)
 posterior.means <- matrix(posterior.means, nrow = 8, ncol = 5, 
  byrow = T)

 par(mfrow = c(1,1))
 matplot(act, t(posterior.means), type = "l", lwd = 3, lty = 1:8, col = 1,
   xlim = c(17, 34))
 legend(30, 3, lty = 1:8, lwd = 2, legend = c("HSR=9", "HSR=8", 
     "HSR=7", "HSR=6", "HSR=5", "HSR=4", "HSR=3", "HSR=2"))

 p <- 1 - pnorm((2.5-FIT$mu)/.65)
 prob.success <- apply(p,2,mean)

 prob.success <- matrix(prob.success, nrow = 8, ncol = 5, byrow = TRUE)
 dimnames(prob.success) <- list(HSR=rlabels,ACTC = clabels)
 round(prob.success,3)




#################################################
# R script for Section 11.4 変化点モデル
#            A Change-Point Model
#################################################

require(arm)

N=112
D=c(4,5,4,1,0,4,3,4,0,6,
3,3,4,0,2,6,3,3,5,4,5,3,1,4,4,1,5,5,3,4,2,5,2,2,3,4,2,1,3,2,
1,1,1,1,1,3,0,0,1,0,1,1,0,0,3,1,0,3,2,2,
0,1,1,1,0,1,0,1,0,0,0,2,1,0,0,0,1,1,0,2,
2,3,1,1,2,1,1,1,1,2,4,2,0,0,0,1,4,0,0,0,
1,0,0,0,0,0,1,0,0,1,0,0)
data=list("N","D")
inits = function() {list(b=c(0,0),changeyear=50)}
parameters = c("changeyear","b")
coalmining.sim = bugs (data, inits, parameters, "coalmining.bug", 
   n.chains=3, n.iter=1000, codaPkg=TRUE)
coalmining.coda = read.bugs(coalmining.sim)
summary(coalmining.coda)
xyplot(coalmining.coda)
acfplot(coalmining.coda)
densityplot(coalmining.coda,col="black")




model
{    
  for( year in 1 : N ) {
       D[year] ~ dpois(mu[year])    
       log(mu[year]) <- b[1] + step(year - changeyear) * b[2]    }
  for (j in 1:2) {b[j] ~ dnorm( 0.0,1.0E-6)}
  changeyear ~ dunif(1,N) 
}


#######################################################
# R script for 11.5  頑健な回帰モデル
#   A Robust Regression Model
#######################################################

require(arm)
require(LearnBayes)

data(election)
attach(election)
y=sqrt(buchanan)
x=sqrt(perot)
N=length(y)

data=list("N","y","x")
inits = function() {list(b=c(0,0),tau=1)}
parameters = c("tau","lam","b")
robust.sim = bugs (data, inits, parameters, "robust.bug", n.chains=3, n.iter=1000)
print(robust.sim)

attach.bugs(robust.sim)
xo=seq(18,196,2)
X0=cbind(1,xo)
meanresponse=b%*%t(X0)
meanp=apply(meanresponse,2,quantile,c(.05,.5,.95))
plot(sqrt(perot),sqrt(buchanan))
lines(xo,meanp[2,])
lines(xo,meanp[1,],lty=2)
lines(xo,meanp[3,],lty=2)



## model code is replaced; thanks for Mr.Satoh 
model
{    
  for( year in 1 : N ) {

    y[i] ~ dnorm (mu [i], p [i])
    p[i] <- tau * lam[i]
    lam [i] ~ dgamma (2,2)
    mu [i] <- b [1] + b[2] * x[i]
  }
  
  for (j in 1:2)
    {
      b[j] ~ dnorm( 0.0, 0.001)
    }

  tau ~ dgamma (0.001, 0.001)
}




#######################################################################
#  R script for Section 11.6  キャリア軌跡を推定する
#  Estimating Career Trajectories
#######################################################################

require(arm)
require(LearnBayes)

data(sluggerdata)
s = careertraj.setup(sluggerdata)
N = s$N;  S = s$T; y = s$y; n = s$n; x = s$x

mean = c(0, 0, 0)
Omega = diag(c(.1,.1,.1))
prec = diag(c(1.0E-6,1.0E-6,1.0E-6))

beta0 = matrix(c(-7.69,.350,-.0058), nrow = 10, ncol = 3, byrow = TRUE)
mu.beta0 = c(-7.69, .350, -.0058)
R0 = diag(c(.1,.1,.1))

data = list("N","S","y","n","x","mean","Omega","prec")
inits = function() {list(beta = beta0, mu.beta = mu.beta0, R = R0)}
parameters = c("beta")
career.sim = bugs (data, inits, parameters, "career.bug", 
   n.chains = 1, n.iter = 10000, n.thin = 1, codaPkg = TRUE)

career.coda = read.bugs(career.sim)
windows(record = TRUE)
plot(career.coda, ask = TRUE)
summary(career.coda)
densityplot(career.coda)



career.sim = bugs (data, inits, parameters, "career.bug", 
   n.chains=1, n.iter=50000, n.thin=1)

peak.age = matrix(0,50000,10)
for (i in 1:10)
peak.age[,i] = -career.sim$sims.list$beta[,i,2]/2/
 career.sim$sims.list$beta[,i,3]

dimnames(peak.age)[[2]] = c("Aaron","Greenberg", "Killebrew", "Mantle","Mays",
      "McCovey" ,"Ott", "Ruth", "Schmidt", "Sosa") 
densityplot(as.mcmc(peak.age), plot.points = FALSE, col = "black", lwd = 2)

summary(as.mcmc(peak.age))

## Quantiles for each variable:

##           2.5%  25%  50%  75%  98%
## Aaron     31.3 32.2 32.8 33.4 34.8
## Greenberg 29.7 31.2 32.1 33.2 35.8
## Killebrew 26.1 27.1 27.6 28.0 28.9
## Mantle    27.1 27.9 28.3 28.7 29.8
## Mays      28.8 29.8 30.2 30.8 31.8
## McCovey   26.8 28.6 29.2 29.7 30.7
## Ott       26.3 27.3 27.8 28.4 29.8
## Ruth      30.1 31.0 31.5 32.0 33.1
## Schmidt   27.6 28.7 29.2 29.7 30.7
## Sosa      30.8 32.1 32.9 33.9 35.9

