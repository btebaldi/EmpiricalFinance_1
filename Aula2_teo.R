# Clear variables
rm(list = ls())

# Used libraries
library(readxl)
library(ggplot2)
library(dplyr)
library(cowplot)
library(xts)
library(PerformanceAnalytics)
library(TSA)
library(fDMA)
library(roll)
library(forecast)

# Used sources
source(file = "ggplot_Acf_Pacf.R")

# Load Database
database <- read_excel("database/dados3_2018.xlsx")

head(database)
# Primeiro  vamos  apresentar a FAC e FACP  dos  quadrados  dos  retornos
# FAC e FACP  dos  quadrados  dos  retornos  do IBOV


p1 <- ggplot(database, aes(x = Data, y = IBOV_SB)) + geom_line(colour="blue") 
p2 <- ggplot(database, aes(x = Data, y = IBRX_SB)) + geom_line(colour="red")
p3 <- ggplot(database, aes(x = Data, y = SP500_SB)) + geom_line(colour="royal blue")
p4 <- ggplot(database, aes(x = Data, y = DJI_SB)) + geom_line(colour="black")


save_plot("plot_1.png", plot_grid(p1, p2, p3, p4))
rm(list = c("p1", "p2", "p3", "p4"))

dados <- database %>% select(IBOV_SB, IBRX_SB, SP500_SB, DJI_SB, Data) %>% arrange(Data)

# Retornos simples e compostos para os ?ndices de Bolsa
dados = dados %>% mutate(rs_ibov = (IBOV_SB - lag(IBOV_SB))/lag(IBOV_SB),
                 rs_ibrx = (IBRX_SB - lag(IBRX_SB))/lag(IBRX_SB),
                 rs_sp500 = (SP500_SB - lag(SP500_SB))/lag(SP500_SB),
                 rs_dji = (DJI_SB - lag(DJI_SB))/lag(DJI_SB),
                 rc_ibov = (log(IBOV_SB) - log(lag(IBOV_SB))),
                 rc_ibrx  = (log(IBRX_SB) - log(lag(IBRX_SB))),
                 rc_sp500 = (log(SP500_SB) - log(lag(SP500_SB))),
                 rc_dji   = (log(DJI_SB) - log(lag(DJI_SB)))
                 )

# Retornos simples e compostos percentuais dos ?ndices de bolsa
dados$rsp_ibov  <- 100 * dados$rs_ibov
dados$rsp_ibrx  <- 100 * dados$rs_ibrx
dados$rsp_sp500 <- 100 * dados$rs_sp500
dados$rsp_dji   <- 100 * dados$rs_dji
dados$rcp_ibov  <- 100 * dados$rc_ibov
dados$rcp_ibrx  <- 100 * dados$rc_ibrx
dados$rcp_sp500 <- 100 * dados$rc_sp500
dados$rcp_dji   <- 100 * dados$rc_dji

dados = dados[-1,]

# Gráfico dos retornos compostos percentuais dos Índices de bolsa
# Para mostrar os gráficos em duas colunas
p1 <- ggplot(dados, aes(x = Data, y = rcp_ibov)) + geom_line(colour="blue") 
p2 <- ggplot(dados, aes(x = Data, y = rcp_ibrx)) + geom_line(colour="red")
p3 <- ggplot(dados, aes(x = Data, y = rcp_sp500)) + geom_line(colour="royal blue")
p4 <- ggplot(dados, aes(x = Data, y = rcp_dji)) + geom_line(colour="black")

save_plot("plot_2.png", plot_grid(p1, p2, p3, p4))
rm(list = c("p1", "p2", "p3", "p4"))


# Agrupamento de volatilidade
dados$rcp_ibov_abs <- abs(dados$rcp_ibov)
dados$rcp_ibov_sq <- dados$rcp_ibov^2


p1 <- ggplot(dados, aes(x = Data, y = rcp_ibov_abs)) + geom_line(colour="black") 
p2 <- ggplot(dados, aes(x = Data, y = rcp_ibov_sq)) + geom_line(colour="black")


save_plot("plot_3.png", plot_grid(p1, p2, ncol=1, nrow=2))
rm(list = c("p1", "p2"))


p1 <- ggplot(dados, aes(x = Data, y = IBOV_SB)) + geom_line(colour="black")
p2 <- ggplot(dados, aes(x = Data, y = rcp_ibov_sq)) + geom_line(colour="black")


save_plot("plot_4.png", plot_grid(p1, p2, ncol=1, nrow=2))
rm(list = c("p1", "p2"))


# Persistência e memória longa observada atravês de FAC e FACP do valor
# absoluto e dos quadrados dos retornos
p1 = ggplot_Acf_Pacf(dados$rcp_ibov_sq)
p2 = ggplot_Acf_Pacf(dados$rcp_ibov_abs)

p1$ACF <- p1$ACF +
  labs(title= "rcp_ibov_sq", # Title
       subtitle=NULL, # Subtitle
       caption=NULL, # Caption
       y="ACF", 
       x='Lag',
       color=NULL)

p2$ACF <- p2$ACF +
  labs(title= "rcp_ibov_abs", # Title
       subtitle=NULL, # Subtitle
       caption=NULL, # Caption
       y="ACF", 
       x='Lag',
       color=NULL)



save_plot("plot_5.png", plot_grid(p1$ACF, p2$ACF, p1$PACF, p2$PACF, ncol=2, nrow=2))
rm(list = c("p1", "p2"))


##
# Ljung- Box para lags 1, 5, 10, 20 e 25 
##
Box.test(dados$rcp_ibov_sq, lag =  1, type =c("Ljung-Box"), fitdf=0)
Box.test(dados$rcp_ibov_sq, lag =  5, type =c("Ljung-Box"), fitdf=0)
Box.test(dados$rcp_ibov_sq, lag = 10, type =c("Ljung-Box"), fitdf=0)
Box.test(dados$rcp_ibov_sq, lag = 20, type =c("Ljung-Box"), fitdf=0)
Box.test(dados$rcp_ibov_sq, lag = 25, type =c("Ljung-Box"), fitdf=0)


##
# Teste de Engle para Heteroscedasticidade
##
##
# como o estimador da constante é a média no modelo de regressão só com a constante
# cria a série dos retornos ao quadrado sem a média que é o resíduo
##
res_ibov_sq_arch = dados$rcp_ibov_sq - mean(dados$rcp_ibov_sq)
##
# faz o teste de ARCH com 12 defasagens
##
fDMA::archtest(ts=as.vector(res_ibov_sq_arch),lag=12);
rm(list =  c("res_ibov_sq_arch"))
##
# desvio padrão móvel com 22, 44, 66, 126, 252
##
rcp_ibov.xts = xts(dados$rcp_ibov, order.by = as.Date(dados$Data))
rcp_ibov_sd_22 = roll::roll_sd(rcp_ibov.xts,22)
rcp_ibov_sd_44 = roll::roll_sd(rcp_ibov.xts,44)
rcp_ibov_sd_66 = roll::roll_sd(rcp_ibov.xts,66)
rcp_ibov_sd_126 = roll::roll_sd(rcp_ibov.xts,126)
rcp_ibov_sd_252 = roll::roll_sd(rcp_ibov.xts,252)

# PerformanceAnalytics::apply.rolling(R = rcp_ibov.xts, width =22, FUN = "sd")


rolling.df = tibble(date = as.Date(dados$Data),
                    sd22 = as.numeric(rcp_ibov_sd_22),
                    sd44 = as.numeric(rcp_ibov_sd_44),
                    sd66 = as.numeric(rcp_ibov_sd_66),
                    sd126 = as.numeric(rcp_ibov_sd_126),
                    sd252 = as.numeric(rcp_ibov_sd_252))

p1 <- ggplot(data = rolling.df, mapping=aes(x=date,y=sd22)) +
  geom_line(colour="black") + 
  labs(title= "SD 22", # Title
       subtitle=NULL, # Subtitle
       caption=NULL, # Caption
       y=NULL, 
       x='Date',
       color=NULL)

p2 <- ggplot(data = rolling.df, mapping=aes(x=date,y=sd44)) +
  geom_line(colour="red") + 
  labs(title= "SD 44", # Title
       subtitle=NULL, # Subtitle
       caption=NULL, # Caption
       y=NULL, 
       x='Date',
       color=NULL)

p3 <- ggplot(data = rolling.df, mapping=aes(x=date,y=sd66)) +
  geom_line(colour="blue") + 
  labs(title= "SD 66", # Title
       subtitle=NULL, # Subtitle
       caption=NULL, # Caption
       y=NULL, 
       x='Date',
       color=NULL)

p4 <- ggplot(data = rolling.df, mapping=aes(x=date,y=sd126)) +
  geom_line(colour="green") + 
  labs(title= "SD 126", # Title
       subtitle=NULL, # Subtitle
       caption=NULL, # Caption
       y=NULL, 
       x='Date',
       color=NULL)

p5 <- ggplot(data = rolling.df, mapping=aes(x=date,y=sd252)) +
  geom_line(colour="royal blue") + 
  labs(title= "SD 252", # Title
       subtitle=NULL, # Subtitle
       caption=NULL, # Caption
       y=NULL, 
       x='Date',
       color=NULL)



save_plot("plot_6.png", plot_grid(p1, p2, p3, p4, p5, ncol=2, nrow=3))


rm(list = c("rcp_ibov_sd_22", "rcp_ibov_sd_44", "rcp_ibov_sd_66", "rcp_ibov_sd_126", "rcp_ibov_sd_252"))
rm(list = c("rolling.df", "rcp_ibov.xts"))
rm(list = c("p1", "p2", "p3", "p4", "p5"))



##
# simulando um ARCH(1) com alpha = 0.9 e omega = 0.1 usando o pacote fGarch 
##
set.seed(123)
garch01.sim = TSA::garch.sim(alpha=c(.1,.9),n=4750)

garch01.df = tibble(garch_sim = garch01.sim, x = 1:length(garch01.sim))
rm(list = c("garch01.sim"))

p1 = ggplot(data = garch01.df, mapping=aes(x=x, y=garch_sim)) +
  geom_line() + 
  labs(title= "Garch Simulation", # Title
       subtitle=NULL, # Subtitle
       caption=NULL, # Caption
       y=expression(r[t]), 
       x='t',
       color=NULL)



save_plot("plot_7.png", p1)
rm(list = c("p1"))




##
# verifica se os dados simulados parecem um ruído branco
##
##
# primeiro transforma os dados numericos numa ts
##
# garch01.sim_ts=ts(garch01.sim)

##
# calcula a FAC e FACP para estes dados
##

p1 = ggplot_Acf_Pacf(garch01.df$garch_sim)

p1$ACF = p1$ACF +   labs(title= "Garch Simulation", # Title
                         subtitle="ACF", # Subtitle
                         caption=NULL, # Caption
                         y=NULL, 
                         x='Lag',
                         color=NULL)

p1$PACF = p1$PACF +   labs(title= "Garch Simulation", # Title
                         subtitle="PACF", # Subtitle
                         caption=NULL, # Caption
                         y=NULL, 
                         x='Lag',
                         color=NULL)


save_plot("plot_8.png", plot_grid(p1$ACF, p1$PACF, ncol=2, nrow=1))
rm(list = c("p1"))


##
# verifica se os dados simulados ao quadrado parecem um ruído branco
##
##
# primeiro transforma os dados para quadrado 
##
garch01.df$garch_sim_sq = garch01.df$garch_sim^2
##
# calcula a FAC e FACP para estes dados
##


p1 = ggplot_Acf_Pacf(garch01.df$garch_sim_sq)

p1$ACF = p1$ACF +   labs(title= "Garch Simulation Squared", # Title
                         subtitle="ACF", # Subtitle
                         caption=NULL, # Caption
                         y=NULL, 
                         x='Lag',
                         color=NULL)

p1$PACF = p1$PACF +   labs(title= "Garch Simulation Squared", # Title
                           subtitle="PACF", # Subtitle
                           caption=NULL, # Caption
                           y=NULL, 
                           x='Lag',
                           color=NULL)

save_plot("plot_9.png", plot_grid(p1$ACF, p1$PACF, ncol=2, nrow=1))
rm(list = c("p1"))


##
# Exponentially Weighted Moving Average (EWMA)
##
dados$rcp_dji_sq = dados$rcp_dji^2
fc <- forecast::ses(dados$rcp_dji_sq, h=1)
summary(fc[["model"]])

EWMA.df = tibble(x = dados$Data, y = as.numeric(fc$fitted))

p1 = ggplot(data = EWMA.df, mapping=aes(x=x, y=y)) +
  geom_line() + 
  labs(title= "Exponentially Weighted Moving Average", # Title
       subtitle=NULL, # Subtitle
       caption=NULL, # Caption
       y=NULL, 
       x='date',
       color=NULL)

save_plot("plot_10.png", p1)
rm(list = c("p1"))

