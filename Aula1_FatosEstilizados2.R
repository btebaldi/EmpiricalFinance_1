
# limpeza de variaveis antigas
rm(list=ls())

# muda diretório para MCF2019 você deve mudar patra o diretório local no seu computador 
setwd("C:/Users/pedro.valls/Dropbox/MCF2019/Lecture0")

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

# Abrindo os dados --------------------------------------------------------
dados<-read.xlsx("C:/Users/pedro.valls/Dropbox/MCF2019/Lecture0/dados3_2018.xlsx", 
                 colNames=TRUE, detectDates = T,
                 na.strings = '#N/A')

# Configurando como série de tempo
attach(dados)
dados<-xts(dados[,-1], order.by = Data)

# Gráfico dos nível dos Índices de bolsa
# Para mostrar os gráficos em duas colunas
par(mfrow=c(2,2)) 
plot(Data, IBOV_SB, ylab='', xlab='time', main='IBOV_SB',type='l', col ="blue")
plot(Data, IBRX_SB, ylab='', xlab='time', main='IBRX_SB',type='l', col="red")
plot(Data, SP500_SB, ylab='', xlab='time', main='SP500_SB',type='l', col = "royal blue")
plot(Data, DJI_SB, ylab='', xlab='time', main='DJI_SB',type='l', col = "black")



# Retornos simples e compostos para os Índices de Bolsa
rsibov<-diff(dados$IBOV_SB)/lag(dados$IBOV_SB)
rsibrx<-diff(dados$IBRX_SB)/lag(dados$IBRX_SB)
rssp500<-diff(dados$SP500_SB)/lag(dados$SP500_SB)
rsdji<-diff(dados$DJI_SB)/lag(dados$DJI_SB)
rcibov<-diff(log(dados$IBOV_SB))[-1]
rcibrx<-diff(log(dados$IBRX_SB))[-1]
rcsp500<-diff(log(dados$SP500_SB))[-1]
rcdji<-diff(log(dados$DJI_SB))[-1]


# Retornos simples e compostos percentuais dos Ìndices de bolsa


rspibov<-100*rsibov
rspibrx<-100*rsibrx
rspsp500<-100*rssp500
rspdji<-100*rsdji
rcpibov<-100*rcibov
rcpibrx<-100*rcibrx
rcpsp500<-100*rcsp500
rcpdji<-100*rcdji



# Gráfico dos retornos compostos percentuais dos Índices de bolsa
# Para mostrar os gráficos em duas colunas
par(mfrow=c(2,2)) 
plot(Data[-1],rcpibov, ylab='', xlab='time', main='RCPIBOV',type='l', col ="blue")
plot(Data[-1], rcpibrx, ylab='', xlab='time', main='RCPIBRX',type='l', col="red")
plot(Data[-1], rcpsp500, ylab='', xlab='time', main='RCPSP500',type='l', col = "royal blue")
plot(Data[-1], rcpdji, ylab='', xlab='time', main='RCPDJI',type='l', col = "black")



# Estatísticas Descritivas para Retornos Compostos Percentuais do IBOV
# cria um data frame com os retornos e chama de rcpall
rcpall<-data.frame(rcpibov,rcpibrx,rcpsp500,rcpdji)
basicStats(rcpall)






# Diagrama de dispersão entre IBOV e IBRX
par(mfrow=c(1,1))
plot(IBOV_SB, IBRX_SB)

# Diagrama de dispersão entre rcpibov e rcpibrx
plot.default(rcpibov, rcpibrx)

# Diagrama de dispersão entre rcpibov e rcpibrx com ajuste linear
plot.default(rcpibov, rcpibrx, main='RCIBOV x RCIBRX - Ajuste Linear')
beta<-coef(lm(rcpibrx~rcpibov))
abline(beta, col='red')

# Diagrama de dispersÃ£o entre rcpibov e rcpibrx com ajuste vizinho mais prÃ³ximo



# Diagrama de dispersão entre rcpibov e rcpibrx com ajuste funÃ§Ã£o nÃºcleo

plot.default(rcpibov, rcpibrx, col='darkblue')
lines(lowess(rcpibov, rcpibrx, f = 2/3, iter = 3, delta = 0.01 * diff(range(rcpibov))), col="red") 
title('Diagrama de Dispersão das Séries - lowess fitted')


# Mudando a periodicidade dos dados para mensal
dados_m<-apply.monthly(dados, mean, na.rm=T)
# time(dados_m)<-as.yearmon(time(dados_m))

# Gerando os retornos compostos mensais em porcentagem
rcpibovm<-100*diff(log(dados_m$IBOV_SB))[-1]
rcpibrxm<-100*diff(log(dados_m$IBRX_SB))[-1]
rcpsp500m<-100*diff(log(dados_m$SP500_SB))[-1]
rcpdjim<-100*diff(log(dados_m$DJI_SB))[-1]

# Padrão sazonal dos retornos mensais

# Criando a funÃ§Ã£o para captar os retornos mensais
seas_plot<-function(data){
  seas<-c()
  means<-c()
  for(i in 1:12){
    # Seleciona apenas o mês i
    seas<-c(seas, data[.indexmon(data)==i-1],
            rep(NA,5))
    # Tira a média do més i
    means<-c(means, rep(mean(data[.indexmon(data)==i-1]),18),
             rep(NA,5))
  }
  plot(seas, type='l', col='blue', xaxt='none', xlab='', ylab='')
  lines(means, col='red')
  axis(side = 1, at=seq(5,length(seas), by=length(seas)/12),
       labels = unique(months(index(data))), las=2)
}

par(mfrow=c(2,2))
seas_plot(rcpibovm)
seas_plot(rcpibrxm)
seas_plot(rcpsp500m)
seas_plot(rcpdjim)


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
d<-density(rcpsp500)
lines(d, col='blue')
legend('topleft', legend=c('RCPDJI','Normal','Kernel'),
       col=c(1,2,4), pch=15)

# QQ-plot dos retornos compostos percentuais
qqnorm(rcpibov)
qqnorm(rcpibrx)
qqnorm(rcpsp500)
qqnorm(rcpdji)

# Estatísticas descritivas dos retornos compostos percentuais
summary(rcpibov)
summary(rcpibrx)
summary(rcpsp500)
summary(rcpdji)

# Covariância entre retornos de ibov e ibrx
cov(rcpibov, rcpibrx)

# Correlação entre retornos de ibov e ibrx
cor(rcpibov, rcpibrx)

# Covariância entre retornos de sp500 e dji
cov(rcpsp500, rcpdji)

# Correlação entre retornos de sp500 e dji
cor(rcpsp500, rcpdji)


# FAC e FACP dos retornos do IBOV
par(mfrow=c(2,1))
acf(rcpibov)
pacf(rcpibov)

# FAC e FACP dos retornos do IBRX
par(mfrow=c(2,1))
acf(rcpibrx)
pacf(rcpibrx)

# FAC e FACP dos retornos do SP500
par(mfrow=c(2,1))
acf(rcpsp500)
pacf(rcpsp500)

# FAC e FACP dos retornos do DJI
par(mfrow=c(2,1))
acf(rcpdji)
pacf(rcpdji)



# Fatos estilizados em dados financeiros
# Caudas pesadas uma forma de verificar é através do Histograma dos retornos da bolsa brasileira   
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
# outra através do qq-plot usando com distribuição teórica a normal
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

# Persistência e memória longa observada atravÃ©s de FAC e FACP do valor
# absoluto e dos quadrados dos retornos
par(mfrow=c(2,2))
acf(rcpibovsq)
acf(absrcpibov)
pacf(rcpibovsq)
pacf(absrcpibov)

# Retornos Extremos aparecem em agrupamentos
## Faz o gráfico das maiores perdas para o RCPIBOV
L <- rcpibov 
# probabilidade usada para determinar as maiores  perdas
r <- 0.01
# determina o quantil-(1-r) empírico
u<- quantile(L,probs=1-r)
# excedentes acima de um quantil empírico escolhido 
xtr.L <- L[L>u]
# gráfico das perdas
plot(as.numeric(xtr.L),type="h", xlab="Time", ylab="Largest Losses") 

# agora considere espaçar os dados (certamente não serão distribuidos exponencialmente)
spcs <- as.numeric(diff(time(xtr.L)))
# r = probabilidade de exceder
qq_plot(spcs, FUN=function(p) qexp(p,rate=r))



# Co-movimentos de volatilidade
rcpibrxsq<-rcpibrx^2
par(mfrow=c(3,1))
plot.default(Data[-1], rcpibovsq, ylab='',
             xlab='', main='RCPIBOVSQ', type='l')
plot.default(Data[-1], rcpibrxsq, ylab='',
             xlab='', main='RCIBRXSQ', type='l')
plot.default(rcpibrxsq, rcpibovsq, main='')