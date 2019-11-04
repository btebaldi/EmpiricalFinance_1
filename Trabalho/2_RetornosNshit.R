# Limpeza de variaveis e graficos antigos
rm(list = ls())

# Bibliotecas utilizadas
# library(dplyr)
# library(xts)
# library(dlm)
library(ggplot2)
# # library(quantmod)
# library(ggExtra)
library(cowplot)
library(TSA)
library(rugarch)

source("./ggplot_Acf_Pacf.R")

# carrega banco de dados
load("./Ibovespa_SemBuracao.RData")

summary(Ibov.data)

# g.caption = sprintf("Bovespa Index from %s, to %s", min(Ibov.data$Date), max(Ibov.data$Date))
g.caption = "Source: Yahoo finance"

ggplot(Ibov.data, aes(x = Date)) + 
  geom_line(aes(y=AdjClose)) + 
  # geom_line(aes(y=Volume), colour = "Red")
  labs(title = "Bovespa Index",
       caption = g.caption,
       y = NULL,
       x = NULL)

ggsave("BovespaIndex_Level.png", plot = last_plot(), device = "png", path = "./trabalho",
       scale = 2, width = 6, height = 3, units = "in",
       dpi = 72)


Ibov.data$r_ibov = log(Ibov.data$AdjClose) - log(dplyr::lag(Ibov.data$AdjClose))

Ibov.data = Ibov.data[-1,]

g.caption = "Source: Yahoo finance"

ggplot(Ibov.data, aes(x = Date)) + 
  geom_line(aes(y=r_ibov)) + 
  # geom_line(aes(y=Volume), colour = "Red")
  labs(title = "Bovespa Index",
       subtitle = "Daily retuns",
       caption = g.caption,
       y = NULL,
       x = NULL)

ggsave("BovespaIndex_Dailyreturns.png", plot = last_plot(), device = "png", path = "./trabalho",
       scale = 2, width = 6, height = 3, units = "in",
       dpi = 72)


ACF_PACF = ggplot_Acf_Pacf(Ibov.data$r_ibov)

plot_grid(ACF_PACF$ACF, ACF_PACF$PACF, label_size = 12, nrow = 2, ncol = 1)

mdl.ARMA = arima(Ibov.data$r_ibov, order =  c(1, 0, 1), fixed = c(NA, NA, NA))
mdl.AR = arima(Ibov.data$r_ibov, order =  c(1, 0, 0))
mdl.MA = arima(Ibov.data$r_ibov, order =  c(0, 0, 1))
mdl = arima(Ibov.data$r_ibov, order =  c(0, 0, 0))


length(mdl.ARMA$residuals)
mdl.ARMA$nobs
mdl.AR$nobs
mdl.MA$nobs
mdl$nobs


ACF_PACF = ggplot_Acf_Pacf(mdl2$residuals)
plot_grid(ACF_PACF$ACF, ACF_PACF$PACF, label_size = 12, nrow = 2, ncol = 1)

# Fit GARCH MODELS

# ARCH(1)
# Especifica ARCH(1) com Distribuicao normal
mdl.arch.norm = ugarchspec(mean.model=list(armaOrder=c(0,0)),
                   variance.model=list(model="sGARCH",garchOrder = c(1, 0)),
                   distribution.model = "norm")

# Especifica ARCH(1) com Distribuicao Skewt-Student
mdl.arch.sstd = ugarchspec(mean.model=list(armaOrder=c(0,0)),
                   variance.model=list(model="sGARCH",garchOrder = c(1, 0)),
                   distribution.model = "sstd")

# GARCH(1,1)
# Especifica GARCH(1,1) com Distribuicao normal
mdl.garch.norm = ugarchspec(mean.model=list(armaOrder=c(0,0)),
                           variance.model=list(model="sGARCH",garchOrder = c(1, 1)),
                           distribution.model = "norm")

# Especifica GARCH(1,1) com Distribuicao Skewt-Student
mdl.garch.sstd = ugarchspec(mean.model=list(armaOrder=c(0,0)),
                           variance.model=list(model="sGARCH",garchOrder = c(1, 1)),
                           distribution.model = "sstd")


# E-GARCH(1,1)
# Especifica E-GARCH(1,1) com Distribuicao normal
mdl.eGarch.norm = ugarchspec(mean.model=list(armaOrder=c(0,0)),
                 variance.model=list(model="eGARCH",garchOrder = c(1, 1)),
                 distribution.model = "norm")


# Especifica E-GARCH(1,1) com Distribuicao Skewt-Student
mdl.eGarch.sstd = ugarchspec(mean.model=list(armaOrder=c(0,0)),
                 variance.model=list(model="eGARCH",garchOrder = c(1, 1)),
                 distribution.model = "sstd")



# gjrGARCH(1,1)
# Especifica gjrGARCH(1,1) com Distribuicao normal
mdl.gjrGarch.norm = ugarchspec(mean.model=list(armaOrder=c(0,0)),
                   variance.model=list(model="gjrGARCH",garchOrder = c(1, 1)),
                   distribution.model = "norm")

# Especifica gjrGARCH(1,1) com Distribuicao Skewt-Student
mdl.gjrGarch.sstd = ugarchspec(mean.model=list(armaOrder=c(0,0)),
                               variance.model=list(model="gjrGARCH",garchOrder = c(1, 1)),
                               distribution.model = "sstd")

# EWMA
# Especifica EWMA com Distribuicao normal
mdl.EWMA.norm = ugarchspec(mean.model=list(armaOrder=c(0,0), include.mean=TRUE),
                   variance.model=list(model="iGARCH", garchOrder=c(1,1)),
                   distribution.model="norm", fixed.pars=list(omega=0))

# Especifica EWMA com Distribuicao Skewt-Student
mdl.EWMA.sstd = ugarchspec(mean.model=list(armaOrder=c(0,0), include.mean=TRUE),
                           variance.model=list(model="iGARCH", garchOrder=c(1,1)),
                           distribution.model="sstd", fixed.pars=list(omega=0))


# apArch
# Especifica apArch(1,1) com Distribuicao normal
spec = ugarchspec(mean.model=list(armaOrder=c(0,0)),
                  variance.model=list(model="apARCH"), garchOrder = c(1,1),
                  distribution.model="norm")

# Especifica apArch(1,1) com Distribuicao Skewt-Student
spec = ugarchspec(mean.model=list(armaOrder=c(0,0)),
                  variance.model=list(model="apARCH"), garchOrder = c(1,1),
                  distribution.model="sstd")

