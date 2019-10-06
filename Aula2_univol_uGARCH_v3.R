
# limpeza de variaveis antigas
rm(list=ls())

# muda diret?rio para MCF2019 voc? deve mudar patra o diret?rio local no seu computador 
# setwd("C:/Users/pedro.valls/Dropbox/MCF2019/Lecture1")

# Pacotes -----------------------------------------------------------------

load_package<-function(x){
  x<-as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x,repos="http://cran.r-project.org")
  }
  require(x,character.only=TRUE)
}

load_package('openxlsx')
load_package('xts')
load_package('class')
load_package('zoo')
load_package('fBasics')
load_package('qrmtools')
load_package('fDMA')
load_package('TSA')
load_package('roll')
load_package('MTS')
load_package('forecast')
load_package(fGarch)
load_package(rugarch)


# Abrindo os dados --------------------------------------------------------
#dados<-read.xlsx("../Dropbox/Educacao/2018 - FGV - Doutorado Academico EESP/Computational Methods in Empirical Finance 1/dados3_2018.xlsx", 
#                 colNames=TRUE, detectDates = T,
#                 na.strings = '#N/A')
dados<-read.xlsx("C:/Users/pedro.valls/Dropbox/MCF2019/Lecture0/dados3_2018.xlsx", 
                 colNames=TRUE, detectDates = T,
                 na.strings = '#N/A')

# Configurando como s?rie de tempo
attach(dados)
dados<-xts(dados[,-1], order.by = Data)

# Gr?fico dos n?vel dos ?ndices de bolsa
# Para mostrar os gr?ficos em duas colunas
par(mfrow=c(2,2)) 
plot(Data, IBOV_SB, ylab='', xlab='time', main='IBOV_SB',type='l', col ="blue")
plot(Data, IBRX_SB, ylab='', xlab='time', main='IBRX_SB',type='l', col="red")
plot(Data, SP500_SB, ylab='', xlab='time', main='SP500_SB',type='l', col = "royal blue")
plot(Data, DJI_SB, ylab='', xlab='time', main='DJI_SB',type='l', col = "black")



# Retornos simples e compostos para os ?ndices de Bolsa
rsibov<-diff(dados$IBOV_SB)/lag(dados$IBOV_SB)
rsibrx<-diff(dados$IBRX_SB)/lag(dados$IBRX_SB)
rssp500<-diff(dados$SP500_SB)/lag(dados$SP500_SB)
rsdji<-diff(dados$DJI_SB)/lag(dados$DJI_SB)
rcibov<-diff(log(dados$IBOV_SB))[-1]
rcibrx<-diff(log(dados$IBRX_SB))[-1]
rcsp500<-diff(log(dados$SP500_SB))[-1]
rcdji<-diff(log(dados$DJI_SB))[-1]


# Retornos simples e compostos percentuais dos ?ndices de bolsa


rspibov<-100*rsibov
rspibrx<-100*rsibrx
rspsp500<-100*rssp500
rspdji<-100*rsdji
rcpibov<-100*rcibov
rcpibrx<-100*rcibrx
rcpsp500<-100*rcsp500
rcpdji<-100*rcdji



# Gr?fico dos retornos compostos percentuais dos ?ndices de bolsa
# Para mostrar os gr?ficos em duas colunas
par(mfrow=c(2,2)) 
plot(Data[-1],rcpibov, ylab='', xlab='time', main='RCPIBOV',type='l', col ="blue")
plot(Data[-1], rcpibrx, ylab='', xlab='time', main='RCPIBRX',type='l', col="red")
plot(Data[-1], rcpsp500, ylab='', xlab='time', main='RCPSP500',type='l', col = "royal blue")
plot(Data[-1], rcpdji, ylab='', xlab='time', main='RCPDJI',type='l', col = "black")




# Histograma dos retornos compostos percentuais do IBOV
par(mfrow=c(1,1))
hist(rcpibov, breaks = 50, freq=F,
     xlab='', ylab='', main='')
dist<-function(n){
  dnorm(n, mean(rcpibov), sd(rcpibov))
}
curve(dist, add=T, col='red')
d<-density(rcpibov)
lines(d, col='blue')
legend('topleft', legend=c('RCPIBOV','Normal','Kernel'),
       col=c(1,2,4), pch=15)

# histograma dos retornos compostos percentuais do IBRX
hist(rcpibrx, breaks = 50, freq=F,
     xlab='', ylab='', main='')
dist<-function(n){
  dnorm(n, mean(rcpibrx), sd(rcpibrx))
}
curve(dist, add=T, col='red')
d<-density(rcpibrx)

lines(d, col='blue')
legend('topleft', legend=c('RCPIBRX','Normal','Kernel'),
       col=c(1,2,4), pch=15)

# histograma dos retornos compostos percentuais do SP500
hist(rcpsp500[-1], breaks = 50, freq=F,
     xlab='', ylab='', main='')
dist<-function(n){
  dnorm(n, mean(rcpsp500), sd(rcpsp500))
}
curve(dist, add=T, col='red')
d<-density(rcpsp500)
lines(d, col='blue')
legend('topleft', legend=c('RCPSP500','Normal','Kernel'),
       col=c(1,2,4), pch=15)

# histograma dos retornos compostos percentuais do DJI
hist(rcpdji[-1], breaks = 50, freq=F,
     xlab='', ylab='', main='')
dist<-function(n){
  dnorm(n, mean(rcpdji), sd(rcpdji))
}
curve(dist, add=T, col='red')
d<-density(rcpdji)
lines(d, col='blue')
legend('topleft', legend=c('RCPDJI','Normal','Kernel'),
       col=c(1,2,4), pch=15)


# Fatos estilizados em dados financeiros
# Caudas pesadas uma forma de verificar ? atrav?s do Histograma dos retornos da bolsa brasileira   
par(mfrow=c(1,1))
hist(rcpibov, breaks = 50, freq=F,
     xlab='', ylab='', main='')
dist<-function(n){
  dnorm(n, mean(rcpibov), sd(rcpibov))
}
curve(dist, add=T, col='red')
d<-density(rcpibov)
lines(d, col='blue')
legend('topleft', legend=c('RCPIBOV','Normal','Kernel'),
       col=c(1,2,4), pch=15)
# outra atrav?s do qq-plot usando com distribui??o te?rica a normal
qq_plot(rcpibov, FUN=qnorm, method = "empirical")

# outra forma testar normalidade usando teste JB
jarqueberaTest(rcpibov)

# Agrupamento de volatilidade
absrcpibov<-abs(rcpibov)
rcpibovsq<-rcpibov*rcpibov
par(mfrow=c(3,1))
plot(absrcpibov, ylab='', xlab='', main='ABSRCPIBOV')
plot(rcpibovsq, ylab='', xlab='', main='RCPIBOVSQ')



# Efeito Alavancagem
par(mfrow=c(2,1))
plot(Data, IBOV_SB, type='l', ylab='', xlab='',
     main='IBOV_SB')
plot.default(index(rcpibovsq), rcpibovsq, type='l',
             ylab='', xlab='', main='RCPIBOVSQ')

# Persist?ncia e mem?ria longa observada atrav?s de FAC e FACP do valor
# absoluto e dos quadrados dos retornos
par(mfrow=c(2,2))
acf(rcpibovsq)
acf(absrcpibov)
pacf(rcpibovsq)
pacf(absrcpibov)

# FAC e FACP dos quadrados dos retornos do IBOV
par(mfrow=c(1,2))
acf(rcpibovsq)
pacf(rcpibovsq)
##
# Ljung- Box para lags 1, 5, 10, 20 e 25 
##
Box.test(rcpibovsq, lag =1, type =c("Ljung-Box"),fitdf=0)
Box.test(rcpibovsq, lag =5, type =c("Ljung-Box"),fitdf=0)
Box.test(rcpibovsq, lag =10, type =c("Ljung-Box"),fitdf=0)
Box.test(rcpibovsq, lag =20, type =c("Ljung-Box"),fitdf=0)
Box.test(rcpibovsq, lag =25, type =c("Ljung-Box"),fitdf=0)

##
# Teste de Engle para Heteroscedasticidade
##
##
# como o estimador da constante ? a m?dia no modelo de regress?o s? com a constante
# cria a s?rie dos retornos ao quadrado sem a m?dia que ? o res?duo
##
res_ibov_sq_arch=rcpibovsq-mean(rcpibovsq)
##
# faz o teste de ARCH com 12 defasagens
##
test1=archtest(ts=as.vector(res_ibov_sq_arch),lag=12);
test1

##
# desvio padr?o m?vel com 22, 44, 66, 126, 252
##
rcpibov_sd_22=roll_sd(rcpibov,22)
rcpibov_sd_44=roll_sd(rcpibov,44)
rcpibov_sd_66=roll_sd(rcpibov,66)
rcpibov_sd_126=roll_sd(rcpibov,126)
rcpibov_sd_252=roll_sd(rcpibov,252)
par(mfrow=c(3,2))
plot(rcpibov_sd_22,type='l', main='SD_22',ylab='',xlab='')
plot(rcpibov_sd_44,type='l', main='SD_44',col='red',ylab='',xlab='')
plot(rcpibov_sd_66,type='l', main='SD_66',col='blue',ylab='',xlab='')
plot(rcpibov_sd_126,type='l', main='SD_126',col='green',ylab='',xlab='')
plot(rcpibov_sd_252,type='l', main='SD_256',col='royal blue',ylab='',xlab='')

##
# simulando um ARCH(1) com alpha = 0.9 e omega = 0.1 usando o pacote fGarch 
##

set.seed(123)
garch01.sim=garch.sim(alpha=c(.1,.9),n=4750)
par(mfrow=c(1,1))
plot(garch01.sim,type='l', main='',ylab=expression(r[t]),xlab='t')

##
# verifica se os dados simulados parecem um ru?do branco
##
##
# primeiro transforma os dados numericos numa ts
##
garch01.sim_ts=ts(garch01.sim)
##
# calcula a FAC e FACP para estes dados
##
par(mfrow=c(1,2))
acf(garch01.sim_ts)
pacf(garch01.sim_ts)

##
# verifica se os dados simulados ao quadrado parecem um ru?do branco
##
##
# primeiro transforma os dados para quadrado 
##
garch01.sim_ts_sq=garch01.sim_ts^2
##
# calcula a FAC e FACP para estes dados
##
par(mfrow=c(1,2))
acf(garch01.sim_ts_sq)
pacf(garch01.sim_ts_sq)

##
# EWMA
##
par(mfrow=c(1,1))
fc<- ses(rcpibovsq,h=1)
summary(fc[["model"]])
plot(Data[-1], fc$fitted, ylab='', xlab='time', main='EWMA',type='l', col = "black")

##
# estimar um arch com dist normal
#
##
# ajusta a m?dia para ter m?dia zero
##
rcpibovm=rcpibov-mean(rcpibov)
##
# Especifica ARCH(1) com Distribui??o Normal
##
spec1=ugarchspec(variance.model=list(model="sGARCH",garchOrder = c(1, 0)),mean.model=list(armaOrder=c(0,0)),distribution.model = "norm")
model_narch=ugarchfit(data=rcpibovm,spec=spec1)
show(model_narch)

##
# salva o desvio padr?o condicional
##
vol_narch<-sigma(model_narch)
##
# gr?fico do SD condicional
##
plot(Data[-1],vol_narch,ylab=' ',xlab=' ',main=' Volatilidade N-ARCH(1)',type='l',col='red')



##
# salva os res?duos n?o padronizados
##
res_narch<-residuals(model_narch)
##
# define os res?duos padronizados = res/h(t)
##
st_res_narch=res_narch/vol_narch
##
# FAC e FACP dos res?duos padronizados
par(mfrow=c(1,2))
acf(st_res_narch)
pacf(st_res_narch)
##
# define os res?duos padronizados ao quadrado
##
st_res_narch_sq=st_res_narch^2
##
# FAC e FACP dos res?duos padronizados ao quadrado
##
par(mfrow=c(1,2))
acf(st_res_narch_sq)
pacf(st_res_narch_sq)
##
# qq plot dos res?duos padronizados
##
par(mfrow=c(1,1))
qq_plot(st_res_narch, FUN=qnorm, method = "empirical",main='qqplot res?duos padronizados N-ARCH(1)')



##
# Especifica ARCH(1) com Distribui??o t-Student
##
spec2=ugarchspec(variance.model=list(model="sGARCH",garchOrder = c(1, 0)),mean.model=list(armaOrder=c(0,0)),distribution.model = "std")
model_tarch=ugarchfit(data=rcpibovm,spec=spec2)
show(model_tarch)

##
# salva o desvio padr?o condicional
##
vol_tarch<-sigma(model_tarch)
##
# gr?fico do SD condicional
##
plot(Data[-1],vol_tarch,ylab=' ',xlab=' ',main=' Volatilidade t-ARCH(1)',type='l',col='red')



##
# salva os res?duos n?o padronizados
##
res_tarch<-residuals(model_tarch)
##
# define os res?duos padronizados = res/h(t)
##
st_res_tarch=res_narch/vol_tarch
##
# FAC e FACP dos res?duos padronizados
par(mfrow=c(1,2))
acf(st_res_tarch)
pacf(st_res_tarch)
##
# define os res?duos padronizados ao quadrado
##
st_res_tarch_sq=st_res_tarch^2
##
# FAC e FACP dos res?duos padronizados ao quadrado
##
par(mfrow=c(1,2))
acf(st_res_tarch_sq)
pacf(st_res_tarch_sq)
##
# qq plot dos res?duos padronizados
##
par(mfrow=c(1,1))
qq_plot(st_res_tarch, FUN=qnorm, method = "empirical",main='qqplot res?duos padronizados t-ARCH(1)')

##
# Especifica ARCH(1) com Distribui??o GED
##
 spec3=ugarchspec(variance.model=list(model="sGARCH",garchOrder = c(1, 0)),mean.model=list(armaOrder=c(0,0)),distribution.model = "ged",start.pars=list(mu=0.00,omega=2.88,alpha1=0.165,shape=1.230145))
 model_gedarch=ugarchfit(data=rcpibovm,spec=spec3)
 show(model_gedarch)

##
# salva o desvio padr?o condicional
##
vol_gedarch<-sigma(model_gedarch)
##
# gr?fico do SD condicional
##
plot(Data[-1],vol_gedarch,ylab=' ',xlab=' ',main=' Volatilidade GED-ARCH(1)',type='l',col='red')



##
# salva os res?duos n?o padronizados
##
res_gedarch<-residuals(model_gedarch)
##
# define os res?duos padronizados = res/h(t)
##
st_res_gedarch=res_gedarch/vol_gedarch
##
# FAC e FACP dos res?duos padronizados
par(mfrow=c(1,2))
acf(st_res_gedarch)
pacf(st_res_gedarch)
##
# define os res?duos padronizados ao quadrado
##
st_res_gedarch_sq=st_res_gedarch^2
##
# FAC e FACP dos res?duos padronizados ao quadrado
##
par(mfrow=c(1,2))
acf(st_res_gedarch_sq)
pacf(st_res_gedarch_sq)
##
# qq plot dos res?duos padronizados
##
par(mfrow=c(1,1))
qq_plot(st_res_gedarch, FUN=qnorm, method = "empirical",main='qqplot res?duos padronizados GED-ARCH(1)')

#
# Especifica ARCH(1) com Distribui??o Skewt-Student
##
spec4=ugarchspec(variance.model=list(model="sGARCH",garchOrder = c(1, 0)),mean.model=list(armaOrder=c(0,0)),distribution.model = "sstd")
model_starch=ugarchfit(data=rcpibovm,spec=spec4)
show(model_starch)

##
# salva o desvio padr?o condicional
##
vol_starch<-sigma(model_starch)
##
# gr?fico do SD condicional
##
plot(Data[-1],vol_starch,ylab=' ',xlab=' ',main=' Volatilidade Skewt-ARCH(1)',type='l',col='red')



##
# salva os res?duos n?o padronizados
##
res_starch<-residuals(model_starch)
##
# define os res?duos padronizados = res/h(t)
##
st_res_starch=res_starch/vol_starch
##
# FAC e FACP dos res?duos padronizados
par(mfrow=c(1,2))
acf(st_res_starch)
pacf(st_res_starch)
##
# define os res?duos padronizados ao quadrado
##
st_res_starch_sq=st_res_starch^2
##
# FAC e FACP dos res?duos padronizados ao quadrado
##
par(mfrow=c(1,2))
acf(st_res_starch_sq)
pacf(st_res_starch_sq)
##
# qq plot dos res?duos padronizados
##
par(mfrow=c(1,1))
qq_plot(st_res_starch, FUN=qnorm, method = "empirical",main='qqplot res?duos padronizados SKEWt-ARCH(1)')


##
# Especifica GARCH(1,1) com Distribuição Normal
##
spec5=ugarchspec(variance.model=list(model="sGARCH",garchOrder = c(1, 1)),mean.model=list(armaOrder=c(0,0)),distribution.model = "norm")
model_ngarch=ugarchfit(data=rcpibovm,spec=spec5)
show(model_ngarch)

##
# salva o desvio padrão condicional
##
vol_ngarch<-sigma(model_ngarch)
##
# gráfico do SD condicional
##
plot(Data[-1],vol_ngarch,ylab=' ',xlab=' ',main=' Volatilidade N-GARCH(1,1)',type='l',col='red')



##
# salva os res????duos não padronizados
##
res_ngarch<-residuals(model_ngarch)
##
# define os res????duos padronizados = res/h(t)
##
st_res_ngarch=res_ngarch/vol_ngarch
##
# FAC e FACP dos res????duos padronizados
par(mfrow=c(1,2))
acf(st_res_ngarch)
pacf(st_res_ngarch)
##
# define os res????duos padronizados ao quadrado
##
st_res_ngarch_sq=st_res_ngarch^2
##
# FAC e FACP dos res????duos padronizados ao quadrado
##
par(mfrow=c(1,2))
acf(st_res_ngarch_sq)
pacf(st_res_ngarch_sq)
##
# qq plot dos res????duos padronizados
##
par(mfrow=c(1,1))
qq_plot(st_res_ngarch, FUN=qnorm, method = "empirical",main='qqplot res?duos padronizados n-garch(1,1)')



##
# Especifica GARCH(1,1) com Distribuição t-Student
##
spec6=ugarchspec(variance.model=list(model="sGARCH",garchOrder = c(1, 1)),mean.model=list(armaOrder=c(0,0)),distribution.model = "std")
model_tgarch=ugarchfit(data=rcpibovm,spec=spec6)
show(model_tgarch)

##
# salva o desvio padrão condicional
##
vol_tgarch<-sigma(model_tgarch)
##
# gráfico do SD condicional
##
plot(Data[-1],vol_tgarch,ylab=' ',xlab=' ',main=' Volatilidade t-GARCH(1,1)',type='l',col='red')



##
# salva os res????duos não padronizados
##
res_tgarch<-residuals(model_tgarch)
##
# define os res????duos padronizados = res/h(t)
##
st_res_tgarch=res_ngarch/vol_tgarch
##
# FAC e FACP dos res????duos padronizados
par(mfrow=c(1,2))
acf(st_res_tgarch)
pacf(st_res_tgarch)
##
# define os res????duos padronizados ao quadrado
##
st_res_tgarch_sq=st_res_tgarch^2
##
# FAC e FACP dos res????duos padronizados ao quadrado
##
par(mfrow=c(1,2))
acf(st_res_tgarch_sq)
pacf(st_res_tgarch_sq)
##
# qq plot dos res????duos padronizados
##
par(mfrow=c(1,1))
qq_plot(st_res_tgarch, FUN=qnorm, method = "empirical",main='qqplot res????duos padronizados t-garch(1,1)')





##
# Especifica GARCH(1,1) com Distribuição GED-Student
##
spec7=ugarchspec(variance.model=list(model="sGARCH",garchOrder = c(1, 1)),mean.model=list(armaOrder=c(0,0)),distribution.model = "ged")
model_gedgarch=ugarchfit(data=rcpibovm,spec=spec7)
show(model_gedgarch)

##
# salva o desvio padrão condicional
##
vol_gedgarch<-sigma(model_gedgarch)
##
# gráfico do SD condicional
##
plot(Data[-1],vol_gedgarch,ylab=' ',xlab=' ',main=' Volatilidade GED-GARCH(1,1)',type='l',col='red')



##
# salva os res????duos não padronizados
##
res_gedgarch<-residuals(model_gedgarch)
##
# define os res????duos padronizados = res/h(t)
##
st_res_gedgarch=res_gedgarch/vol_gedgarch
##
# FAC e FACP dos res????duos padronizados
par(mfrow=c(1,2))
acf(st_res_gedgarch)
pacf(st_res_gedgarch)
##
# define os res????duos padronizados ao quadrado
##
st_res_gedgarch_sq=st_res_gedgarch^2
##
# FAC e FACP dos res????duos padronizados ao quadrado
##
par(mfrow=c(1,2))
acf(st_res_gedgarch_sq)
pacf(st_res_gedgarch_sq)
##
# qq plot dos res????duos padronizados
##
par(mfrow=c(1,1))
qq_plot(st_res_gedgarch, FUN=qnorm, method = "empirical",main='qqplot res????duos padronizados ged-garch(1,1)')




##
# Especifica GARCH(1,1) com Distribuição Skewt-Student
##
spec8=ugarchspec(variance.model=list(model="sGARCH",garchOrder = c(1, 1)),mean.model=list(armaOrder=c(0,0)),distribution.model = "sstd")
model_stgarch=ugarchfit(data=rcpibovm,spec=spec8)
show(model_stgarch)

##
# salva o desvio padrão condicional
##
vol_stgarch<-sigma(model_stgarch)
##
# gráfico do SD condicional
##
plot(Data[-1],vol_stgarch,ylab=' ',xlab=' ',main=' Volatilidade Skewt-GARCH(1,1)',type='l',col='red')



##
# salva os res????duos não padronizados
##
res_stgarch<-residuals(model_stgarch)
##
# define os res????duos padronizados = res/h(t)
##
st_res_stgarch=res_stgarch/vol_stgarch
##
# FAC e FACP dos res????duos padronizados
par(mfrow=c(1,2))
acf(st_res_stgarch)
pacf(st_res_stgarch)
##
# define os res????duos padronizados ao quadrado
##
st_res_stgarch_sq=st_res_stgarch^2
##
# FAC e FACP dos res????duos padronizados ao quadrado
##
par(mfrow=c(1,2))
acf(st_res_stgarch_sq)
pacf(st_res_stgarch_sq)
##
# qq plot dos res????duos padronizados
##
par(mfrow=c(1,1))
qq_plot(st_res_stgarch, FUN=qnorm, method = "empirical",main='qqplot res????duos padronizados SKEWt-garch(1,1)')



##
# Especifica EGARCH(1,1) com Distribui??o Normal
##
spec9=ugarchspec(variance.model=list(model="eGARCH",garchOrder = c(1, 1)),mean.model=list(armaOrder=c(0,0)),distribution.model = "norm")
model_negarch=ugarchfit(data=rcpibovm,spec=spec9)
show(model_negarch)

##
# salva o desvio padr?o condicional
##
vol_negarch<-sigma(model_negarch)
##
# gr?fico do SD condicional
##
plot(Data[-1],vol_negarch,ylab=' ',xlab=' ',main=' Volatilidade N-EGARCH(1,1)',type='l',col='red')



##
# salva os res?duos n?o padronizados
##
res_negarch<-residuals(model_negarch)
##
# define os res?duos padronizados = res/h(t)
##
st_res_negarch=res_negarch/vol_negarch
##
# FAC e FACP dos res?duos padronizados
par(mfrow=c(1,2))
acf(st_res_negarch)
pacf(st_res_negarch)
##
# define os res?duos padronizados ao quadrado
##
st_res_negarch_sq=st_res_negarch^2
##
# FAC e FACP dos res?duos padronizados ao quadrado
##
par(mfrow=c(1,2))
acf(st_res_negarch_sq)
pacf(st_res_negarch_sq)
##
# qq plot dos res?duos padronizados
##
par(mfrow=c(1,1))
qq_plot(st_res_negarch, FUN=qnorm, method = "empirical",main='qqplot res?duos padronizados N-EGARCH(1,1)')


##
# Especifica EGARCH(1,1) com Distribui??o t-Student
##
spec10=ugarchspec(variance.model=list(model="eGARCH",garchOrder = c(1, 1)),mean.model=list(armaOrder=c(0,0)),distribution.model = "std")
model_tegarch=ugarchfit(data=rcpibovm,spec=spec10)
show(model_tegarch)

##
# salva o desvio padr?o condicional
##
vol_tegarch<-sigma(model_tegarch)
##
# gr?fico do SD condicional
##
plot(Data[-1],vol_tegarch,ylab=' ',xlab=' ',main=' Volatilidade t-EGARCH(1,1)',type='l',col='red')

# ##
# # salva o desvio padr?o condicional
# ##
# vol_tegarch<-sigma(model_tegarch)
# ##
# # gr?fico do SD condicional
# ##
# plot(Data[-1],vol_tegarch,ylab=' ',xlab=' ',main=' Volatilidade t-EGARCH(1,1)',type='l',col='red')



##
# salva os res?duos n?o padronizados
##
res_tegarch<-residuals(model_tegarch)
##
# define os res?duos padronizados = res/h(t)
##
st_res_tegarch=res_tegarch/vol_tegarch
##
# FAC e FACP dos res?duos padronizados
par(mfrow=c(1,2))
acf(st_res_tegarch)
pacf(st_res_tegarch)
##
# define os res?duos padronizados ao quadrado
##
st_res_tegarch_sq=st_res_tegarch^2
##
# FAC e FACP dos res?duos padronizados ao quadrado
##
par(mfrow=c(1,2))
acf(st_res_tegarch_sq)
pacf(st_res_tegarch_sq)
##
# qq plot dos res?duos padronizados
##
par(mfrow=c(1,1))
qq_plot(st_res_tegarch, FUN=qnorm, method = "empirical",main='qqplot res?duos padronizados t-EGARCH(1,1)')



##
# Especifica EGARCH(1,1) com Distribui??o GED
##
spec11=ugarchspec(variance.model=list(model="eGARCH",garchOrder = c(1, 1)),mean.model=list(armaOrder=c(0,0)),distribution.model = "ged")
model_gedegarch=ugarchfit(data=rcpibovm,spec=spec11)
show(model_gedegarch)

##
# salva o desvio padr?o condicional
##
vol_gedegarch<-sigma(model_gedegarch)
##
# gr?fico do SD condicional
##
plot(Data[-1],vol_gedegarch,ylab=' ',xlab=' ',main=' Volatilidade GED-EGARCH(1,1)',type='l',col='red')



##
# salva os res?duos n?o padronizados
##
res_gedegarch<-residuals(model_gedegarch)
##
# define os res?duos padronizados = res/h(t)
##
st_res_gedegarch=res_gedegarch/vol_gedegarch
##
# FAC e FACP dos res?duos padronizados
par(mfrow=c(1,2))
acf(st_res_gedegarch)
pacf(st_res_gedegarch)
##
# define os res?duos padronizados ao quadrado
##
st_res_gedegarch_sq=st_res_gedegarch^2
##
# FAC e FACP dos res?duos padronizados ao quadrado
##
par(mfrow=c(1,2))
acf(st_res_gedegarch_sq)
pacf(st_res_gedegarch_sq)
##
# qq plot dos res?duos padronizados
##
par(mfrow=c(1,1))
qq_plot(st_res_gedegarch, FUN=qnorm, method = "empirical",main='qqplot res?duos padronizados GED-EGARCH(1,1)')



##
# Especifica EGARCH(1,1) com Distribui??o SKW-T
##
spec12=ugarchspec(variance.model=list(model="eGARCH",garchOrder = c(1, 1)),mean.model=list(armaOrder=c(0,0)),distribution.model = "sstd")
model_stegarch=ugarchfit(data=rcpibovm,spec=spec12)
show(model_stegarch)

##
# salva o desvio padr?o condicional
##
vol_stegarch<-sigma(model_stegarch)
##
# gr?fico do SD condicional
##
plot(Data[-1],vol_stegarch,ylab=' ',xlab=' ',main=' Volatilidade SKW-t-EGARCH(1,1)',type='l',col='red')



##
# salva os res?duos n?o padronizados
##
res_stegarch<-residuals(model_stegarch)
##
# define os res?duos padronizados = res/h(t)
##
st_res_stegarch=res_stegarch/vol_stegarch
##
# FAC e FACP dos res?duos padronizados
par(mfrow=c(1,2))
acf(st_res_stegarch)
pacf(st_res_stegarch)
##
# define os res?duos padronizados ao quadrado
##
st_res_stegarch_sq=st_res_stegarch^2
##
# FAC e FACP dos res?duos padronizados ao quadrado
##
par(mfrow=c(1,2))
acf(st_res_stegarch_sq)
pacf(st_res_stegarch_sq)
##
# qq plot dos res?duos padronizados
##
par(mfrow=c(1,1))
qq_plot(st_res_stegarch, FUN=qnorm, method = "empirical",main='qqplot res?duos padronizados SKW-t-EGARCH(1,1)')


##
# Especifica iGARCH(1,1) com Distribui??o Normal
##
spec13=ugarchspec(variance.model=list(model="iGARCH",garchOrder = c(1, 1)),mean.model=list(armaOrder=c(0,0)),distribution.model = "norm")
model_nigarch=ugarchfit(data=rcpibovm,spec=spec13)
show(model_nigarch)

##
# salva o desvio padr?o condicional
##




vol_nigarch<-sigma(model_nigarch)
##
# gr?fico do SD condicional
##
plot(Data[-1],vol_nigarch,ylab=' ',xlab=' ',main=' Volatilidade N-IGARCH(1,1)',type='l',col='red')



##
# salva os res?duos n?o padronizados
##
res_nigarch<-residuals(model_nigarch)
##
# define os res?duos padronizados = res/h(t)
##
st_res_nigarch=res_nigarch/vol_nigarch
##
# FAC e FACP dos res?duos padronizados
par(mfrow=c(1,2))
acf(st_res_nigarch)
pacf(st_res_nigarch)
##
# define os res?duos padronizados ao quadrado
##
st_res_nigarch_sq=st_res_nigarch^2
##
# FAC e FACP dos res?duos padronizados ao quadrado
##
par(mfrow=c(1,2))
acf(st_res_nigarch_sq)
pacf(st_res_nigarch_sq)
##
# qq plot dos res?duos padronizados
##
par(mfrow=c(1,1))
qq_plot(st_res_nigarch, FUN=qnorm, method = "empirical",main='qqplot res?duos padronizados N-IGARCH(1,1)')


##
# Especifica iGARCH(1,1) com Distribui??o t-Student
##
spec14=ugarchspec(variance.model=list(model="iGARCH",garchOrder = c(1, 1)),mean.model=list(armaOrder=c(0,0)),distribution.model = "std")
model_tigarch=ugarchfit(data=rcpibovm,spec=spec14)
show(model_tigarch)

##
# salva o desvio padr?o condicional
##
vol_tigarch<-sigma(model_tigarch)
##
# gr?fico do SD condicional
##
plot(Data[-1],vol_tigarch,ylab=' ',xlab=' ',main=' Volatilidade t-IGARCH(1,1)',type='l',col='red')

# ##
# # salva o desvio padr?o condicional
# ##
# vol_tigarch<-sigma(model_tigarch)
# ##
# # gr?fico do SD condicional
# ##
# plot(Data[-1],vol_tigarch,ylab=' ',xlab=' ',main=' Volatilidade t-IGARCH(1,1)',type='l',col='red')



##
# salva os res?duos n?o padronizados
##
res_tigarch<-residuals(model_tigarch)
##
# define os res?duos padronizados = res/h(t)
##
st_res_tigarch=res_tigarch/vol_tigarch
##
# FAC e FACP dos res?duos padronizados
par(mfrow=c(1,2))
acf(st_res_tigarch)
pacf(st_res_tigarch)
##
# define os res?duos padronizados ao quadrado
##
st_res_tigarch_sq=st_res_tigarch^2
##
# FAC e FACP dos res?duos padronizados ao quadrado
##
par(mfrow=c(1,2))
acf(st_res_tigarch_sq)
pacf(st_res_tigarch_sq)
##
# qq plot dos res?duos padronizados
##
par(mfrow=c(1,1))
qq_plot(st_res_tigarch, FUN=qnorm, method = "empirical",main='qqplot res?duos padronizados t-IGARCH(1,1)')



##
# Especifica iGARCH(1,1) com Distribui??o GED
##
spec15=ugarchspec(variance.model=list(model="iGARCH",garchOrder = c(1, 1)),mean.model=list(armaOrder=c(0,0)),distribution.model = "ged")
model_gedigarch=ugarchfit(data=rcpibovm,spec=spec15)
show(model_gedigarch)

##
# salva o desvio padr?o condicional
##
vol_gedigarch<-sigma(model_gedigarch)
##
# gr?fico do SD condicional
##
plot(Data[-1],vol_gedigarch,ylab=' ',xlab=' ',main=' Volatilidade GED-IGARCH(1,1)',type='l',col='red')



##
# salva os res?duos n?o padronizados
##
res_gedigarch<-residuals(model_gedigarch)
##
# define os res?duos padronizados = res/h(t)
##
st_res_gedigarch=res_gedigarch/vol_gedigarch
##
# FAC e FACP dos res?duos padronizados
par(mfrow=c(1,2))
acf(st_res_gedigarch)
pacf(st_res_gedigarch)
##
# define os res?duos padronizados ao quadrado
##
st_res_gedigarch_sq=st_res_gedigarch^2
##
# FAC e FACP dos res?duos padronizados ao quadrado
##
par(mfrow=c(1,2))
acf(st_res_gedigarch_sq)
pacf(st_res_gedigarch_sq)
##
# qq plot dos res?duos padronizados
##
par(mfrow=c(1,1))
qq_plot(st_res_gedigarch, FUN=qnorm, method = "empirical",main='qqplot res?duos padronizados GED-IGARCH(1,1)')



##
# Especifica iGARCH(1,1) com Distribui??o SKW-T
##
spec16=ugarchspec(variance.model=list(model="iGARCH",garchOrder = c(1, 1)),mean.model=list(armaOrder=c(0,0)),distribution.model = "sstd")
model_stigarch=ugarchfit(data=rcpibovm,spec=spec16)
show(model_stigarch)

##
# salva o desvio padr?o condicional
##
vol_stigarch<-sigma(model_stigarch)
##
# gr?fico do SD condicional
##
plot(Data[-1],vol_stigarch,ylab=' ',xlab=' ',main=' Volatilidade SKW-t-IGARCH(1,1)',type='l',col='red')



##
# salva os res?duos n?o padronizados
##
res_stigarch<-residuals(model_stigarch)
##
# define os res?duos padronizados = res/h(t)
##
st_res_stigarch=res_stigarch/vol_stigarch
##
# FAC e FACP dos res?duos padronizados
par(mfrow=c(1,2))
acf(st_res_stigarch)
pacf(st_res_stigarch)
##
# define os res?duos padronizados ao quadrado
##
st_res_stigarch_sq=st_res_stigarch^2
##
# FAC e FACP dos res?duos padronizados ao quadrado
##
par(mfrow=c(1,2))
acf(st_res_stigarch_sq)
pacf(st_res_stigarch_sq)
##
# qq plot dos res?duos padronizados
##
par(mfrow=c(1,1))
qq_plot(st_res_stigarch, FUN=qnorm, method = "empirical",main='qqplot res?duos padronizados SKW-t-IGARCH(1,1)')




##
# Especifica gjrGARCH(1,1) com Distribui??o Normal
##
spec17=ugarchspec(variance.model=list(model="gjrGARCH",garchOrder = c(1, 1)),mean.model=list(armaOrder=c(0,0)),distribution.model = "norm")
model_ngjrGARCH=ugarchfit(data=rcpibovm,spec=spec17)
show(model_ngjrGARCH)

##
# salva o desvio padr?o condicional
##
vol_ngjrGARCH<-sigma(model_ngjrGARCH)
##
# gr?fico do SD condicional
##
plot(Data[-1],vol_ngjrGARCH,ylab=' ',xlab=' ',main=' Volatilidade N-gjrGARCH(1,1)',type='l',col='red')



##
# salva os res?duos n?o padronizados
##
res_ngjrGARCH<-residuals(model_ngjrGARCH)
##
# define os res?duos padronizados = res/h(t)
##
st_res_ngjrGARCH=res_ngjrGARCH/vol_ngjrGARCH
##
# FAC e FACP dos res?duos padronizados
par(mfrow=c(1,2))
acf(st_res_ngjrGARCH)
pacf(st_res_ngjrGARCH)
##
# define os res?duos padronizados ao quadrado
##
st_res_ngjrGARCH_sq=st_res_ngjrGARCH^2
##
# FAC e FACP dos res?duos padronizados ao quadrado
##
par(mfrow=c(1,2))
acf(st_res_ngjrGARCH_sq)
pacf(st_res_ngjrGARCH_sq)
##
# qq plot dos res?duos padronizados
##
par(mfrow=c(1,1))
qq_plot(st_res_ngjrGARCH, FUN=qnorm, method = "empirical",main='qqplot res?duos padronizados N-gjrGARCH(1,1)')



##
# Especifica gjrGARCH(1,1) com Distribui??o t-Student
##
spec18=ugarchspec(variance.model=list(model="gjrGARCH",garchOrder = c(1, 1)),mean.model=list(armaOrder=c(0,0)),distribution.model = "std")
model_tgjrGARCH=ugarchfit(data=rcpibovm,spec=spec18)
show(model_tgjrGARCH)

##
# salva o desvio padr?o condicional
##
vol_tgjrGARCH<-sigma(model_tgjrGARCH)
##
# gr?fico do SD condicional
##
plot(Data[-1],vol_tgjrGARCH,ylab=' ',xlab=' ',main=' Volatilidade t-gjrGARCH(1,1)',type='l',col='red')

# ##
# # salva o desvio padr?o condicional
# ##
# vol_tgjrGARCH<-sigma(model_tgjrGARCH)
# ##
# # gr?fico do SD condicional
# ##
# plot(Data[-1],vol_tgjrGARCH,ylab=' ',xlab=' ',main=' Volatilidade t-gjrGARCH(1,1)',type='l',col='red')



##
# salva os res?duos n?o padronizados
##
res_tgjrGARCH<-residuals(model_tgjrGARCH)
##
# define os res?duos padronizados = res/h(t)
##
st_res_tgjrGARCH=res_tgjrGARCH/vol_tgjrGARCH
##
# FAC e FACP dos res?duos padronizados
par(mfrow=c(1,2))
acf(st_res_tgjrGARCH)
pacf(st_res_tgjrGARCH)
##
# define os res?duos padronizados ao quadrado
##
st_res_tgjrGARCH_sq=st_res_tgjrGARCH^2
##
# FAC e FACP dos res?duos padronizados ao quadrado
##
par(mfrow=c(1,2))
acf(st_res_tgjrGARCH_sq)
pacf(st_res_tgjrGARCH_sq)
##
# qq plot dos res?duos padronizados
##
par(mfrow=c(1,1))
qq_plot(st_res_tgjrGARCH, FUN=qnorm, method = "empirical",main='qqplot res?duos padronizados t-gjrGARCH(1,1)')




##
# Especifica gjrGARCH(1,1) com Distribui??o GED
##
spec19=ugarchspec(variance.model=list(model="gjrGARCH",garchOrder = c(1, 1)),mean.model=list(armaOrder=c(0,0)),distribution.model = "ged",
                  start.pars=list(mu=0.00,omega=0.0038,aplha1=0.0452,beta1=0.9272,gamma1=0.0411,shape=1.477))
model_gedgjrGARCH=ugarchfit(data=rcpibovm,spec=spec19)
show(model_gedgjrGARCH)

##
# salva o desvio padr?o condicional
##
vol_gedgjrGARCH<-sigma(model_gedgjrGARCH)
##
# gr?fico do SD condicional
##
plot(Data[-1],vol_gedgjrGARCH,ylab=' ',xlab=' ',main=' Volatilidade GED-gjrGARCH(1,1)',type='l',col='red')



##
# salva os res?duos n?o padronizados
##
res_gedgjrGARCH<-residuals(model_gedgjrGARCH)
##
# define os res?duos padronizados = res/h(t)
##
st_res_gedgjrGARCH=res_gedgjrGARCH/vol_gedgjrGARCH
##
# FAC e FACP dos res?duos padronizados
par(mfrow=c(1,2))
acf(st_res_gedgjrGARCH)
pacf(st_res_gedgjrGARCH)
##
# define os res?duos padronizados ao quadrado
##
st_res_gedgjrGARCH_sq=st_res_gedgjrGARCH^2
##
# FAC e FACP dos res?duos padronizados ao quadrado
##
par(mfrow=c(1,2))
acf(st_res_gedgjrGARCH_sq)
pacf(st_res_gedgjrGARCH_sq)
##
# qq plot dos res?duos padronizados
##
par(mfrow=c(1,1))
qq_plot(st_res_gedgjrGARCH, FUN=qnorm, method = "empirical",main='qqplot res?duos padronizados GED-gjrGARCH(1,1)')





##
# Especifica gjrGARCH(1,1) com Distribui??o SKW-T
##
spec20=ugarchspec(variance.model=list(model="gjrGARCH",garchOrder = c(1, 1)),mean.model=list(armaOrder=c(0,0)),distribution.model = "sstd",
                  start.pars=list(mu=0.00,omega=0.0037,alpha1=0.0451,beta1=0.92,gamma1=0.044,skew=0.923,shape=8.30))
model_stgjrGARCH=ugarchfit(data=rcpibovm,spec=spec20)
show(model_stgjrGARCH)

##
# salva o desvio padr?o condicional
##
vol_stgjrGARCH<-sigma(model_stgjrGARCH)
##
# gr?fico do SD condicional
##
plot(Data[-1],vol_stgjrGARCH,ylab=' ',xlab=' ',main=' Volatilidade SKW-t-gjrGARCH(1,1)',type='l',col='red')



##
# salva os res?duos n?o padronizados
##
res_stgjrGARCH<-residuals(model_stgjrGARCH)
##
# define os res?duos padronizados = res/h(t)
##
st_res_stgjrGARCH=res_stgjrGARCH/vol_stgjrGARCH
##
# FAC e FACP dos res?duos padronizados
par(mfrow=c(1,2))
acf(st_res_stgjrGARCH)
pacf(st_res_stgjrGARCH)
##
# define os res?duos padronizados ao quadrado
##
st_res_stgjrGARCH_sq=st_res_stgjrGARCH^2
##
# FAC e FACP dos res?duos padronizados ao quadrado
##
par(mfrow=c(1,2))
acf(st_res_stgjrGARCH_sq)
pacf(st_res_stgjrGARCH_sq)
##
# qq plot dos res?duos padronizados
##
par(mfrow=c(1,1))
qq_plot(st_res_stgjrGARCH, FUN=qnorm, method = "empirical",main='qqplot res?duos padronizados SKW-t-gjrGARCH(1,1)')





##
# compara os IC para todos os modelos
##


infocriteria(model_narch)
infocriteria(model_tarch)
infocriteria(model_gedarch)
infocriteria(model_starch)
infocriteria(model_ngarch)
infocriteria(model_tgarch)
infocriteria(model_gedgarch)
infocriteria(model_stgarch)
infocriteria(model_negarch)
infocriteria(model_tegarch)
infocriteria(model_gedegarch)
infocriteria(model_stegarch)
infocriteria(model_nigarch)
infocriteria(model_tigarch)
infocriteria(model_gedigarch)
infocriteria(model_stigarch)
infocriteria(model_ngjrGARCH)
infocriteria(model_tgjrGARCH)
infocriteria(model_gedgjrGARCH)
infocriteria(model_stgjrGARCH)


