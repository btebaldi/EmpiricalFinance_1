##
# Implementing risk forecasting
##
##
# tseries is a package for time series and computational finance
# for help type library(help="tseries")
##
# clean old variables
#
rm(list=ls())

load_package<-function(x){
  x<-as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x,repos="http://cran.r-project.org")
  }
  require(x,character.only=TRUE)
}

load_package('tseries')
load_package('xlsx')
# need to used QRM
##
load_package('QRM')

##
# need some library
##
library(xts) # for time series manipulation
library(mvtnorm) # for sampling from a multivariate normal or t distribution
library(qrmdata) # for Dow Jones constituents data
library(qrmtools) # for returns()
library(QRM) # for fit.mst() 

## 
# dowload Microsoft prices and IBM 
##

con <- url("https://finance.yahoo.com")
if(!inherits(try(open(con), silent = TRUE), "try-error")) {
close(con)
x_msft <- get.hist.quote(instrument = "msft", start = "2000-01-01",end="2009-12-31",quote = "Adjusted")
plot(x_msft,main = "Microsoft", col = 'red',ylab='AdjClose',xlab='')
x_ibm_price <- get.hist.quote(instrument = "ibm", start = "2000-01-01",end="2009-12-31",quote = "Adjusted")
x_ibm_vol <- get.hist.quote(instrument = "ibm", start = "2000-01-01",end="2009-12-31", quote = "Vol")
# par(mfrow=c(1,1))
plot(x_ibm_price, xlab='',ylab='AdjClose', main="IBM Prices", col='blue')
plot(x_ibm_vol, xlab='',ylab='Vol', main="IBM Volume", col='red')
# plot(x, main = "International Business Machines Corp")
msft <- get.hist.quote(instrument = "msft", start = "2000-01-01", end="2009-12-31",quote = "Adjusted")
ibm <- get.hist.quote(instrument = "ibm",  start = "2000-01-01",  end="2009-12-31",quote = "Adjusted")
require("zoo")		# For merge() method.
par(mfrow=c(1,1))
plot(msft, main = "MSFT", col='red',xlab='',ylab='AdjClose')
plot(ibm, main = "IBM", col='red',xlab='',ylab='AdjClose')
}
##
# convert prices in percentual log returns
##
r_msft=coredata(diff(log(msft)))
r_ibm=coredata(diff(log(ibm)))
##
# plot the returns
##
par(mfrow=c(2,1))
plot(r_msft, main = "MSFT", xlab='',ylab='returns',type='l',col='blue')
plot(r_ibm, main = "IBM", col='red',xlab='',ylab='returns',type='l')
##
# Drop first 14 observations
##
r_msft=tail(r_msft,T-14)
r_ibm=tail(r_ibm,T-14)
##
##

# New sample size
##
T = length(r_msft)

##
# Portfolio Value##
##
value = 1000
##
# merge returns in one matrix
##
r = cbind(r_msft,r_ibm)
##
# probability for V@R 
##
p = 0.01 

##
# Univariate HS for MSFT and IBM
##
r_msft_sort = sort(r_msft)
r_ibm_sort=sort(r_ibm)
##
# p% smallest
##
op = T*p
##
# VaR for MSFT
##
VaR1_msft=-r_msft_sort[op]*value
print(VaR1_msft)
##
# VaR for IBM
##
VaR1_ibm=-r_ibm_sort[op]*value
print(VaR1_ibm) 

##
# Multivariate HS 
##
##
# vector of portfolio weights
#
w = matrix(c(0.3,0.7))
##
# obtain portfolio returns
##
r_port = r%*%w
r_port_sort = sort(r_port)
VaR2_port=-r_port_sort[op]*value
print(VaR2_port)

##
# Univariate ES for MSFT
##
ES1_msft=-mean(r_msft_sort[1:op])*value
print(ES1_msft)
##
# Univariate ES for IBM
##
ES1_ibm=-mean(r_ibm_sort[1:op])*value
print(ES1_ibm)
##
# Univariate ES for Portfolio
##
ES1_port=-mean(r_port_sort[1:op])*value
print(ES1_port)
##
# Normal V@R for MSFT
##
## 
# first estimate the volatility using sample standard deviation
##
sigma_msft=sd(r_msft)
##
# compute $\sigma$*$F^{-1}(p)$*Value
##
VaR3_msft=-sigma_msft*qnorm(p)*value
print(VaR3_msft)


##
# Normal V@R for IBM
##
## 
# first estimate the volatility using sample standard deviation
##
sigma_ibm=sd(r_ibm)
##
# compute $\sigma$*$F^{-1}(p)$*Value
##
VaR3_ibm=-sigma_ibm*qnorm(p)*value
print(VaR3_ibm)

##
# Normal V@R for Portfolio
##
## 
# first estimate the portfolio volatility 
##
sigma_port=sqrt(t(w)%*%cov(r)%*%w)
##
# compute $\sigma$*$F^{-1}(p)$*Value
##
VaR3_port=-sigma_port*qnorm(p)*value
print(VaR3_port)

##
# V@R assuming Student-t distribution
##
##
# first we need to estimate the degrees of freedom of the distribution
##
##
# need to used QRM
##

##
# scale the returns for MSFT
##
sc_r_msft=r_msft*100
##
# estimate the distribution parameters
##
res_msft=fit.st(sc_r_msft)
##
# rescale the volatility
##
sigma_sc_r_msft=res_msft$par.ests[3]/100
##
# extract the degrees of freedom
#
nu_sc_r_msft=res_msft$par.ests[1]
##
# calculates the V@R
##
VaR5_r_msft=-sigma_sc_r_msft*qt(df=nu_sc_r_msft,p=p)*value
print(VaR5_r_msft)

##
# scale the returns for IBM
##
sc_r_ibm=r_ibm*100
##
# estimate the distribution parameters
##
res_ibm=fit.st(sc_r_ibm)
##
# rescale the volatility
##
sigma_sc_r_ibm=res_ibm$par.ests[3]/100
##
# extract the degrees of freedom
#
nu_sc_r_ibm=res_ibm$par.ests[1]
##
# calculates the V@R
##
VaR5_r_ibm=-sigma_sc_r_ibm*qt(df=nu_sc_r_ibm,p=p)*value
print(VaR5_r_ibm)




## Fitting a multivariate normal distribution to r and simulating from it
mu <- colMeans(r) # estimated location vector
Sigma <- cov(r) # estimated scale matrix
stopifnot(all.equal(Sigma, var(r)))
P <- cor(r) # estimated correlation matrix
stopifnot(all.equal(P, cov2cor(Sigma)))
n <- nrow(r) # sample size
set.seed(271)
r.norm <- rmvnorm(n, mean = mu, sigma = Sigma) # N(mu, Sigma) samples

## Fitting a multivariate t distribution to r
fit <- fit.mst(r, method = "BFGS") # fit a multivariate t distribution
r.t <- rmvt(n, sigma = as.matrix(fit$Sigma), df = fit$df, delta = fit$mu) # t_nu samples

## Plot (sample from fitted t (red), original sample (black), sample from fitted normal (blue))
dat <- rbind(t = r.t, original = as.matrix(r), norm = r.norm)
cols <- rep(c("maroon3", "black", "royalblue3"), each = n)
pairs(dat, gap = 0, pch = ".", col = cols)

##
# Portfolio Student-t V@R
##
sigma_mt=sqrt(t(w)%*%fit$covariance%*%w)
VaR5_r_mt=-sigma_mt*qt(p,fit$df)*value
print(VaR5_r_mt)


# Histogram of log returns of MSFT and Normal distribution 
par(mfrow=c(1,1))
hist(r_msft, breaks = 50, freq=F,
     xlab='', ylab='', main='')
dist<-function(n){
  dnorm(n, mean(r_msft), sd(r_msft))
}
curve(dist, add=TRUE, col='red')
d<-density(r_msft)
lines(d, col='blue')
legend('topleft', legend=c('RCMSFT','Normal','Kernel'),
       col=c(1,2,4), pch=15)


##
# Normal ES for MSFT
#
ES2_msft=sigma_msft*dnorm(qnorm(p))/p*value
print(ES2_msft)

##
# Normal ES for IBM
#
ES2_ibm=sigma_ibm*dnorm(qnorm(p))/p*value
print(ES2_ibm)

##
# Normal ES for Portfolio
##

ES_portfolio_normal=sigma_port*dnorm(qnorm(p))/p*value
print(ES_portfolio_normal)


##
# MA normal VaR in R for MSFT 
##
VaR6_MA_msft<-rep(NA,length(r_msft))
WE=20
for (t in seq(WE+1,length(r_msft))){
  t1=t-WE+1
  window= r_msft[t1:t] # estimation window
  sigma=sd(window)
  VaR6 = -sigma * qnorm(p) * value
  VaR6_MA_msft[t]=VaR6
}

##
# MA normal VaR and VaR(1%)
##
##
# create a vector for VaR(1%) with the same value for all entrances
##
par(mfrow=c(2,1))
VaR1p_msft<-rep(VaR3_msft,length(r_msft))
##
# create x as time for graph
x<-c(1:2500)
##
# plot VaR6_MA_msft and VaR(1%) in same graph
##
matplot(x,cbind(VaR6_MA_msft,VaR1p_msft),type='l',pch=19,ylab="",main="VaR for MSFT using MA(20) and Normal-VaR(1%)")
legend('top', legend=c('VaR_{MA}','VaR_{1%}'),col=1:2,bty='n',pch=15)
plot(x,r_msft,type='l',ylab="",main="Return of MSFT")


##
# MA normal VaR in R for IBM 
##
VaR6_MA_ibm<-rep(NA,length(r_ibm))
WE=20
for (t in seq(WE+1,length(r_ibm))){
  t1=t-WE+1
  window= r_ibm[t1:t] # estimation window
  sigma=sd(window)
  VaR6 = -sigma * qnorm(p) * value
  VaR6_MA_ibm[t]=VaR6
}

##
# MA normal VaR and VaR(1%)
##
##
# create a vector for VaR(1%) with the same value for all entrances
##
par(mfrow=c(2,1))
VaR1p_ibm<-rep(VaR3_ibm,length(r_ibm))
##
# create x as time for graph
x<-c(1:2500)
##
# plot VaR6_MA_ibm and VaR(1%) in same graph
##
matplot(x,cbind(VaR6_MA_ibm,VaR1p_ibm),type='l',pch=19,ylab="",main="VaR for IBM using MA(20) and Normal-VaR(1%)")
legend('top', legend=c('VaR_{MA}','VaR_{1%}'),col=1:2,bty='n',pch=15)
plot(x,r_ibm,type='l',ylab="",main="Return of IBM")

##
# MA normal VaR in R for portfolio 
##
VaR6_MA_port<-rep(NA,length(r_ibm))
WE=20
for (t in seq(WE+1,length(r_ibm))){
  t1=t-WE+1
  window= r_port[t1:t] # estimation window
  sigma=sd(window)
  VaR6 = -sigma * qnorm(p) * value
  VaR6_MA_port[t]=VaR6
}

##
# MA normal VaR and VaR(1%)
##
##
# create a vector for VaR(1%) with the same value for all entrances
##
par(mfrow=c(2,1))
VaR1p_port<-rep(VaR3_port,length(r_ibm))
##
# create x as time for graph
x<-c(1:2500)
##
# plot VaR6_MA_port and VaR(1%) in same graph
##
matplot(x,cbind(VaR6_MA_port,VaR1p_port),type='l',pch=19,ylab="",main="VaR for Portfolio using MA(20) and Normal-VaR(1%)")
legend('top', legend=c('VaR_{MA}','VaR_{1%}'),col=1:2,bty='n',pch=15)
plot(x,r_port,type='l',ylab="",main="Return of Portfolio")

##
# EWMA VaR for MSFT
##
VaR7_EWMA_msft<-rep(NA,length(r_msft))
lambda=0.94;
s11=var(r_msft[1:30]);
for (t in seq(2,length(r_msft))){
 s11=lambda*s11+(1-lambda)*r_msft[t-1]^2
   VaR7_EWMA_msft[t]=-sqrt(s11)*qnorm(p)*value
}
##
# plot VaR7_EWMA_msft and VaR(1%) in same graph
##
par(mfrow=c(3,1))
matplot(x,cbind(VaR7_EWMA_msft,VaR1p_msft),type='l',pch=19,ylab="",main="VaR for MSFT using EWMA and Normal-VaR(1%)")
legend('top', legend=c('VaR_{EWMA}','VaR_{1%}'),col=1:2,bty='n',pch=15)
plot(x,r_msft,type='l',ylab="",main="Return of MSFT",col="blue")
plot.default(VaR6_MA_msft,VaR7_EWMA_msft,ylab="",main="Scatter plot VaR_MA and VaR_EWMA for MSFT")
beta<-coef(lm(VaR7_EWMA_msft~VaR6_MA_msft))
abline(beta, col='red')


##
# EWMA VaR for IBM
##
VaR7_EWMA_ibm<-rep(NA,length(r_ibm))
lambda=0.94;
s11=var(r_ibm[1:30]);
for (t in seq(2,length(r_ibm))){
  s11=lambda*s11+(1-lambda)*r_ibm[t-1]^2
  VaR7_EWMA_ibm[t]=-sqrt(s11)*qnorm(p)*value
}

##
# plot VaR7_EWMA_IBM and VaR(1%) in same graph
##
par(mfrow=c(3,1))
matplot(x,cbind(VaR7_EWMA_ibm,VaR1p_ibm),type='l',pch=19,ylab="",main="VaR for IBM using EWMA and Normal-VaR(1%)")
legend('top', legend=c('VaR_{EWMA}','VaR_{1%}'),col=1:2,bty='n',pch=15)
plot(x,r_ibm,type='l',ylab="",main="Return of IBM",col="blue")
plot.default(VaR6_MA_ibm,VaR7_EWMA_ibm,ylab="",main="Scatter plot VaR_MA and VaR_EWMA for IBM")
beta<-coef(lm(VaR7_EWMA_ibm~VaR6_MA_ibm))
abline(beta, col='red')



##
# EWMA VaR for Portfolio 
##
VaR8_EWMA_port<-rep(NA,length(r_ibm))

s=cov(r)
for (t in seq(2,length(r_ibm))){
  s=lambda*s+(1-lambda)*r[t-1,]%*%t(r[t-1,])
sigma=sqrt(t(w)%*%s%*%w)
VaR8_EWMA_port[t]=-sigma*qnorm(p)*value
}

##
# plot VaR8_EWMA_port and VaR(1%) in same graph
##
par(mfrow=c(2,1))
matplot(x,cbind(VaR8_EWMA_port,VaR2_port),type='l',pch=19,ylab="",main="VaR for Portfolio using EWMA and Normal-VaR(1%)")
legend('top', legend=c('VaR_{EWMA}','VaR_{1%}'),col=1:2,bty='n',pch=15)
plot(x,r_port,type='l',ylab="",main="Return of Portfolio",col="blue")




library(rugarch)
VaR9_GARCH_msft<-rep(NA,length(r_msft))
VaR9_GJR_msft<-rep(NA,length(r_msft))

spec = ugarchspec(variance.model = list( garchOrder = c(1, 1)),
                  mean.model = list( armaOrder = c(0,0),
                                     include.mean = FALSE))

spec2 = ugarchspec(variance.model=list(model="gjrGARCH", garchOrder = c(1, 1)),
                   mean.model=list(armaOrder=c(0,0)),
                   distribution.model = "norm")


res = ugarchfit(spec = spec, data = r_msft)
# Information Criteria
# ------------------------------------
#   
# Akaike       -5.1069
# Bayes        -5.0999
# Shibata      -5.1069
# Hannan-Quinn -5.1043

res2 = ugarchfit(spec = spec2, data = r_msft)
# Information Criteria
# ------------------------------------
#   
# Akaike       -5.1172
# Bayes        -5.1056
# Shibata      -5.1173
# Hannan-Quinn -5.1130

omega = res@fit$coef[1]
alpha = res@fit$coef[2]
beta = res@fit$coef[3]

mu2 = res2@fit$coef["mu"]
omega2 = res2@fit$coef["omega"]
alpha2 = res2@fit$coef["alpha1"]
beta2  = res2@fit$coef["beta1"]
gamma2  = res2@fit$coef["gamma1"]


plot(res2@fit$fitted.values)
# for (t in seq(2,length(r_ibm))){
#   sigma2 = omega + alpha*(r_msft[t-1]^2) + beta*(res@fit$var[t-1])
#    VaR9_GARCH_msft[t]=-sqrt(sigma2)*qnorm(p)*value
# }

for (t in seq(2,length(r_ibm))){
  if(r_ibm[t-1] >= mu2)
  {
    i =1
  }else{
    i=0
  }
  
  sigma2 = omega2 + (alpha2 + gamma2*i)*(r_msft[t-1]^2) + beta2*(res@fit$var[t-1])
  VaR9_GJR_msft[t]=-sqrt(sigma2)*qnorm(p)*value
}

##
# plot VaR9_GARCH_msft and VaR(1%) in same graph
##
# par(mfrow=c(3,1))
library(ggplot2)

# plot(VaR9_GJR_msft)
VaR9_GJR_msft.df = data.frame(VaR9_GJR_msft)
ggplot(VaR9_GJR_msft.df, aes(x=1:nrow(VaR9_GJR_msft.df), y=VaR9_GJR_msft)) + 
  geom_line() +
  geom_line(data = as.data.frame(VaR1p_msft), aes(y=VaR1p_msft), colour="red", linetype = "dashed") +
  labs(x = "Mito", y="Variance") +
  theme_bw()
  
  
matplot(x,cbind(VaR9_GARCH_msft,VaR1p_msft),type='l',pch=19,ylab="",main="VaR for MSFT using GARCH and Normal-VaR(1%)")
legend('top', legend=c('VaR_{GARCH}','VaR_{1%}'),col=1:2,bty='n',pch=15)


plot(x,r_msft,type='l',ylab="",main="Return of MSFT",col="blue")
plot.default(VaR7_EWMA_msft,VaR9_GARCH_msft,ylab="",main="Scatter plot VaR_EWMA and VaR_GARCH for MSFT")
beta<-coef(lm(VaR9_GARCH_msft~VaR7_EWMA_msft))
abline(beta, col='red')

0.84134

