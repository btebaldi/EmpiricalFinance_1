# Limpeza de variaveis e graficos antigos
rm(list = ls())

# Bibliotecas utilizadas
library(dplyr)
library(xts)
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


mdl.ARMA$aic
mdl.AR$aic
mdl.MA$aic
mdl$aic


# Fit GARCH MODELS
models = list()

# ARCH(1)
# Especifica ARCH(1) com Distribuicao normal
models$arch.norm = ugarchspec(mean.model=list(armaOrder=c(0,0)),
                   variance.model=list(model="sGARCH",garchOrder = c(1, 0)),
                   distribution.model = "norm")

# Especifica ARCH(1) com Distribuicao Skewt-Student
models$arch.sstd = ugarchspec(mean.model=list(armaOrder=c(0,0)),
                   variance.model=list(model="sGARCH",garchOrder = c(1, 0)),
                   distribution.model = "sstd")

# GARCH(1,1)
# Especifica GARCH(1,1) com Distribuicao normal
models$garch.norm = ugarchspec(mean.model=list(armaOrder=c(0,0)),
                           variance.model=list(model="sGARCH",garchOrder = c(1, 1)),
                           distribution.model = "norm")

# Especifica GARCH(1,1) com Distribuicao Skewt-Student
models$garch.sstd = ugarchspec(mean.model=list(armaOrder=c(0,0)),
                           variance.model=list(model="sGARCH",garchOrder = c(1, 1)),
                           distribution.model = "sstd")


# E-GARCH(1,1)
# Especifica E-GARCH(1,1) com Distribuicao normal
models$eGarch.norm = ugarchspec(mean.model=list(armaOrder=c(0,0)),
                 variance.model=list(model="eGARCH",garchOrder = c(1, 1)),
                 distribution.model = "norm")


# Especifica E-GARCH(1,1) com Distribuicao Skewt-Student
models$eGarch.sstd = ugarchspec(mean.model=list(armaOrder=c(0,0)),
                 variance.model=list(model="eGARCH",garchOrder = c(1, 1)),
                 distribution.model = "sstd")



# gjrGARCH(1,1)
# Especifica gjrGARCH(1,1) com Distribuicao normal
models$gjrGarch.norm = ugarchspec(mean.model=list(armaOrder=c(0,0)),
                   variance.model=list(model="gjrGARCH",garchOrder = c(1, 1)),
                   distribution.model = "norm")

# Especifica gjrGARCH(1,1) com Distribuicao Skewt-Student
models$gjrGarch.sstd = ugarchspec(mean.model=list(armaOrder=c(0,0)),
                               variance.model=list(model="gjrGARCH",garchOrder = c(1, 1)),
                               distribution.model = "sstd")

# EWMA
# Especifica EWMA com Distribuicao normal
models$EWMA.norm = ugarchspec(mean.model=list(armaOrder=c(0,0), include.mean=TRUE),
                   variance.model=list(model="iGARCH", garchOrder=c(1,1)),
                   distribution.model="norm", fixed.pars=list(omega=0))

# Especifica EWMA com Distribuicao Skewt-Student
models$EWMA.sstd = ugarchspec(mean.model=list(armaOrder=c(0,0), include.mean=TRUE),
                           variance.model=list(model="iGARCH", garchOrder=c(1,1)),
                           distribution.model="sstd", fixed.pars=list(omega=0))


# apArch(1,1)
# Especifica apArch(1,1) com Distribuicao normal
models$apArch.norm = ugarchspec(mean.model=list(armaOrder=c(0,0)),
                  variance.model=list(model="apARCH", garchOrder = c(1,1)),
                  distribution.model="norm")

# Especifica apArch(1,1) com Distribuicao Skewt-Student
models$apArch.sstd = ugarchspec(mean.model=list(armaOrder=c(0,0)),
                  variance.model=list(model="apARCH", garchOrder = c(1,1)),
                  distribution.model="sstd")



# Faz a selecao dos dados para estimação (< 2013)
Ibov.data.InSample = Ibov.data %>% dplyr::filter(Date < "2013-01-01")

r_ibov.xts = xts::xts(Ibov.data.InSample$r_ibov, order.by = Ibov.data.InSample$Date)
# ForecastWindow1 = nrow(Ibov.data) - nrow(Ibov.data.InSample)


# Fitted list
fit = vector(mode = "list", length = length(models))
names(fit) = names(models)

# Fit the sample 
for (i in 1:length(models)) {
  fit[[i]] = ugarchfit(data = r_ibov.xts, spec = models[[i]])
}

# Fit on sample data as a hole no forecasting
# fit$arch.norm = ugarchfit(data = r_ibov.xts, spec = mdl.arch.norm)
# fit$arch.sstd = ugarchfit(data = r_ibov.xts, spec = mdl.arch.sstd)
# fit$garch.norm = ugarchfit(data = r_ibov.xts, spec = mdl.garch.norm)
# fit$garch.sstd = ugarchfit(data = r_ibov.xts, spec = mdl.garch.sstd)
# fit$eGarch.norm = ugarchfit(data = r_ibov.xts, spec = mdl.eGarch.norm)
# fit$eGarch.sstd = ugarchfit(data = r_ibov.xts, spec = mdl.eGarch.sstd)
# fit$gjrGarch.norm = ugarchfit(data = r_ibov.xts, spec = mdl.gjrGarch.norm)
# fit$gjrGarch.sstd = ugarchfit(data = r_ibov.xts, spec = mdl.gjrGarch.sstd)
# fit$EWMA.norm = ugarchfit(data = r_ibov.xts, spec = mdl.EWMA.norm)
# fit$EWMA.sstd = ugarchfit(data = r_ibov.xts, spec = mdl.EWMA.sstd)
# fit$apArch.norm = ugarchfit(data = r_ibov.xts, spec = mdl.apArch.norm)
# fit$apArch.sstd = ugarchfit(data = r_ibov.xts, spec = mdl.apArch.sstd)

# Imprime os graficos dos fits.
for (i in 1:length(fit)) {
  p1 = ggplot(Ibov.data.InSample, aes(x=Date)) +
    geom_line(aes(y = r_ibov), colour =  "grey") +
    geom_line(aes(y = sigma(fit[[i]])), colour =  "blue") +
    geom_line(aes(y = -sigma(fit[[i]])), colour =  "blue") +
    theme_bw()  
  
  ggplot2::ggsave(paste(i, "Returns.png", sep = "_"), plot = p1)
}
rm(list = c("p1"))
# 
# 
# ggplot(Ibov.data.InSample, aes(x=Date)) +
#   geom_line(aes(y = r_ibov), colour =  "grey") +
#   geom_line(aes(y = sigma(fit.garch.norm)), colour =  "blue") +
#   geom_line(aes(y = -sigma(fit.garch.norm)), colour =  "blue") +
#   theme_bw()
# 
# ggplot(Ibov.data.InSample, aes(x=Date)) +
#   geom_line(aes(y = r_ibov), colour =  "grey") +
#   geom_line(aes(y = sigma(fit.eGarch.norm)), colour =  "blue") +
#   geom_line(aes(y = -sigma(fit.eGarch.norm)), colour =  "blue") +
#   theme_bw()
# 
# ggplot(Ibov.data.InSample, aes(x=Date)) +
#   geom_line(aes(y = r_ibov), colour =  "grey") +
#   geom_line(aes(y = sigma(fit.gjrGarch.norm)), colour =  "blue") +
#   geom_line(aes(y = -sigma(fit.gjrGarch.norm)), colour =  "blue") +
#   theme_bw()
# 
# ggplot(Ibov.data.InSample, aes(x=Date)) +
#   geom_line(aes(y = r_ibov), colour =  "grey") +
#   geom_line(aes(y = sigma(fit.EWMA.norm)), colour =  "blue") +
#   geom_line(aes(y = -sigma(fit.EWMA.norm)), colour =  "blue") +
#   theme_bw()
# 
# ggplot(Ibov.data.InSample, aes(x=Date)) +
#   geom_line(aes(y = r_ibov), colour =  "grey") +
#   geom_line(aes(y = sigma(fit.apArch.norm)), colour =  "blue") +
#   geom_line(aes(y = -sigma(fit.apArch.norm)), colour =  "blue") +
#   theme_bw()

Models = c("ARCH Norm", "ARCH Sstd", "GARCH Norm", "GARCH Sstd", "E-GARCH Norm", "E-GARCH Sstd", 
           "Gjr-Garch Norm", "Gjr-Garch Sstd", "EWMA Norm", "EWMA Sstd", "apArch Norm", "apArch Sstd")
tb_InfoCriteria = tibble(Model = Models, fit = NA, Akaike = NA, Bayes = NA, Shibata=NA, HannanQuinn=NA)

for (i in 1:length(fit)) {
  tb_InfoCriteria[i, -c(1,2)] = t(rugarch::infocriteria(fit[[i]]))
  tb_InfoCriteria[i, 2] = names(fit[i])
}

# *************** Rolling Window Estimation ***************

# Determina a partir de quando vai comecar o forecast
# 2013-01-02 = 3295
n.startForecast = nrow(Ibov.data.InSample)+1

# Reconstroi a serie XTS, pois agora vamos usar roling window forecast
r_ibov.xts = xts::xts(Ibov.data$r_ibov, order.by = Ibov.data$Date)

# refit.window	c("recursive", "moving")
# Whether the refit is done on an expanding window including all the previous data or
# a moving window where all previous data is used for the first estimation and then 
# moved by a length equal to refit.every (unless the window.size option is used instead).

# Determina qual o periodo de reestimacao
# Expanding window recalculation every 5 days
fit.roll_5 = vector(mode = "list", length = length(models))
names(fit.roll_5) = names(models)
for (i in 1:length(models)) {
  cat(sprintf("Fitting: %s\n",names(fit.roll_5[i])))
  fit.roll_5[[i]] = ugarchroll(spec = models[[i]],
                               data = r_ibov.xts,
                               n.start = n.startForecast,
                               refit.window = "recursive",
                               refit.every = 5)
}

# Expanding window recalculation every 20 days
fit.roll_20 = vector(mode = "list", length = length(models))
names(fit.roll_20) = names(models)
for (i in 1:length(models)) {
  cat(sprintf("Fitting: %s\n",names(fit.roll_20[i])))
  fit.roll_20[[i]] = ugarchroll(spec = models[[i]],
                               data = r_ibov.xts,
                               n.start = n.startForecast,
                               refit.window = "recursive",
                               refit.every = 20)
}

# Expanding window recalculation every 60 days
fit.roll_60 = vector(mode = "list", length = length(models))
names(fit.roll_60) = names(models)
for (i in 1:length(models)) {
  cat(sprintf("Fitting: %s\n",names(fit.roll_60[i])))
  fit.roll_60[[i]] = ugarchroll(spec = models[[i]],
                                data = r_ibov.xts,
                                n.start = n.startForecast,
                                refit.window = "recursive",
                                refit.every = 60)
}


# Expanding window recalculation every 252 days
fit.roll_252 = vector(mode = "list", length = length(models))
names(fit.roll_252) = names(models)
for (i in 1:length(models)) {
  cat(sprintf("Fitting: %s\n",names(fit.roll_252[i])))  
  fit.roll_252[[i]] = ugarchroll(spec = models[[i]],
                                data = r_ibov.xts,
                                n.start = n.startForecast,
                                refit.window = "recursive",
                                refit.every = 252)
}

# *************** MOVING WINDOW (2 year) ***************

# Determina a partir de quando vai comecar o forecast

# (2 year) => 2013 - 2 = 2011
Ibov.data.rolling = Ibov.data %>% dplyr::filter(Date >= "2011-01-01")

# 2013-01-02 = 494
n.startForecast = nrow(Ibov.data.InSample %>% dplyr::filter(Date >= "2011-01-01"))+1

# Reconstroi a serie XTS, pois agora vamos usar roling window forecast
r_ibov.xts = xts::xts(Ibov.data.rolling$r_ibov, order.by = Ibov.data.rolling$Date)

# refit.window	c("recursive", "moving")
# Whether the refit is done on an expanding window including all the previous data or
# a moving window where all previous data is used for the first estimation and then 
# moved by a length equal to refit.every (unless the window.size option is used instead).

# Determina qual o periodo de reestimacao
# Expanding window recalculation every 5 days
fit.mov2_5 = vector(mode = "list", length = length(models))
names(fit.mov2_5) = names(models)
for (i in 1:length(models)) {
  cat(sprintf("Fitting: %s\n",names(fit.mov2_5[i])))
  fit.mov2_5[[i]] = ugarchroll(spec = models[[i]],
                               data = r_ibov.xts,
                               n.start = n.startForecast,
                               refit.window = "moving",
                               refit.every = 5)
}

# Expanding window recalculation every 20 days
fit.mov2_20 = vector(mode = "list", length = length(models))
names(fit.mov2_20) = names(models)
for (i in 1:length(models)) {
  cat(sprintf("Fitting: %s\n",names(fit.mov2_20[i])))
  fit.mov2_20[[i]] = ugarchroll(spec = models[[i]],
                                data = r_ibov.xts,
                                n.start = n.startForecast,
                                refit.window = "moving",
                                refit.every = 20)
}

# Expanding window recalculation every 60 days
fit.mov2_60 = vector(mode = "list", length = length(models))
names(fit.mov2_60) = names(models)
for (i in 1:length(models)) {
  cat(sprintf("Fitting: %s\n",names(fit.mov2_60[i])))
  fit.mov2_60[[i]] = ugarchroll(spec = models[[i]],
                                data = r_ibov.xts,
                                n.start = n.startForecast,
                                refit.window = "moving",
                                refit.every = 60)
}


# Expanding window recalculation every 252 days
fit.mov2_252 = vector(mode = "list", length = length(models))
names(fit.mov2_252) = names(models)
for (i in 1:length(models)) {
  cat(sprintf("Fitting: %s\n",names(fit.mov2_252[i])))
  fit.mov2_252[[i]] = ugarchroll(spec = models[[i]],
                                data = r_ibov.xts,
                                n.start = n.startForecast,
                                refit.window = "moving",
                                refit.every = 252)
}


# *************** MOVING WINDOW (5 year) ***************


# Determina a partir de quando vai comecar o forecast

# (5 year) => 2013 - 5 = 2008
Ibov.data.rolling = Ibov.data %>% dplyr::filter(Date >= "2008-01-01")

# 2013-01-02 = 1239
n.startForecast = nrow(Ibov.data.InSample %>% dplyr::filter(Date >= "2008-01-01")) + 1

# Reconstroi a serie XTS, pois agora vamos usar roling window forecast
r_ibov.xts = xts::xts(Ibov.data.rolling$r_ibov, order.by = Ibov.data.rolling$Date)

# refit.window	c("recursive", "moving")
# Whether the refit is done on an expanding window including all the previous data or
# a moving window where all previous data is used for the first estimation and then 
# moved by a length equal to refit.every (unless the window.size option is used instead).

# Determina qual o periodo de reestimacao
# Expanding window recalculation every 5 days
fit.mov5_5 = vector(mode = "list", length = length(models))
names(fit.mov5_5) = names(models)
for (i in 1:length(models)) {
  cat(sprintf("Fitting: %s\nTime frame: 5 years\n\n", names(fit.mov5_5[i])))
  fit.mov5_5[[i]] = ugarchroll(spec = models[[i]],
                               data = r_ibov.xts,
                               n.start = n.startForecast,
                               refit.window = "moving",
                               refit.every = 5)
}

# Expanding window recalculation every 20 days
fit.mov5_20 = vector(mode = "list", length = length(models))
names(fit.mov5_20) = names(models)
for (i in 1:length(models)) {
  cat(sprintf("Fitting: %s\nTime frame: 5 years\n\n",names(fit.mov5_20[i])))
  fit.mov5_20[[i]] = ugarchroll(spec = models[[i]],
                                data = r_ibov.xts,
                                n.start = n.startForecast,
                                refit.window = "moving",
                                refit.every = 20)
}

# Expanding window recalculation every 60 days
fit.mov5_60 = vector(mode = "list", length = length(models))
names(fit.mov5_60) = names(models)
for (i in 1:length(models)) {
  cat(sprintf("Fitting: %s\nTime frame: 5 years\n\n",names(fit.mov5_60[i])))
  fit.mov5_60[[i]] = ugarchroll(spec = models[[i]],
                                data = r_ibov.xts,
                                n.start = n.startForecast,
                                refit.window = "moving",
                                refit.every = 60)
}


# Expanding window recalculation every 252 days
fit.mov5_252 = vector(mode = "list", length = length(models))
names(fit.mov5_252) = names(models)
for (i in 1:length(models)) {
  cat(sprintf("Fitting: %s\n",names(fit.mov5_252[i])))
  fit.mov5_252[[i]] = ugarchroll(spec = models[[i]],
                                 data = r_ibov.xts,
                                 n.start = n.startForecast,
                                 refit.window = "moving",
                                 refit.every = 252)
}

