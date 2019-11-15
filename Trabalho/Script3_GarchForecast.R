# Limpeza de variaveis e graficos antigos
rm(list = ls())

# Bibliotecas utilizadas
library(dplyr)
library(xts)
library(ggplot2)
library(cowplot)
library(TSA)
library(rugarch)

source("./Trabalho/ggplot_Acf_Pacf.R")

# carrega banco de dados
load("./Trabalho/Database/Ibovespa_SemBuracao.RData")

# Estatisticas descritivas
summary(Ibov.data)

#  Calcula o retorno da serie
Ibov.data$r_ibov = log(Ibov.data$AdjClose) - log(dplyr::lag(Ibov.data$AdjClose))

# Desarta a primeira observação
Ibov.data = Ibov.data[-1,]

# Gera o grafico dos retornos
g.caption = "Source: Yahoo finance"

g1 = ggplot(Ibov.data, aes(x = Date)) + 
  geom_line(aes(y=r_ibov)) + 
  labs(title = "Bovespa Index",
       subtitle = "Daily retuns",
       caption = g.caption,
       y = NULL,
       x = NULL)

ggsave("BovespaIndex_Dailyreturns.png", plot = g1, device = "png", path = "./Trabalho/Plots/",
       scale = 2, width = 6, height = 3, units = "in",
       dpi = 72)

# Gera o grafico de ACF e PACF dos Retornos
ACF_PACF = ggplot_Acf_Pacf(Ibov.data$r_ibov)

g1 = cowplot::plot_grid(ACF_PACF$ACF, ACF_PACF$PACF, label_size = 12, nrow = 2, ncol = 1)

g1 = g1 + labs(title = "Bovespa Index",
               subtitle = "Daily retuns",
               caption = g.caption,
               y = NULL,
               x = NULL)

ggsave("ACF_PACF.png", plot = g1, device = "png", path = "./Trabalho/Plots/",
       scale = 1.5, width = 6, height = 3, units = "in",
       dpi = 72)

# remove o objeto de grafico da memoria
rm(list = c("g1", "ACF_PACF"))

# Determina modelos ARIMA para saber o comportamento da serie
mdl.ARMA = arima(Ibov.data$r_ibov, order =  c(1, 0, 1), fixed = c(NA, NA, NA))
mdl.AR = arima(Ibov.data$r_ibov, order =  c(1, 0, 0))
mdl.MA = arima(Ibov.data$r_ibov, order =  c(0, 0, 1))
mdl = arima(Ibov.data$r_ibov, order =  c(0, 0, 0))

# Imprime (em tela) o AIC de cada modelo
mdl.ARMA$aic
mdl.AR$aic
mdl.MA$aic
mdl$aic

cat(sprintf("ARMA(1,1) %7.2f\nARMA(1,0) %7.2f\nARMA(0,1) %7.2f\nARMA(0,0) %7.2f",
            mdl.ARMA$aic, mdl.AR$aic, mdl.ARMA$aic, mdl$aic))

# Cocnlusao: o modelo ARMA(0,0) sera utilizado para modelagem dos retornos
# remove os modelos da memoria
rm(list = c("mdl.ARMA", "mdl.AR", "mdl.MA", "mdl"))

# *************** GARCH MODELS ***************

# Declara lista que contera todos os modelos
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

# determina a serie como xts
r_ibov.xts = xts::xts(Ibov.data.InSample$r_ibov, order.by = Ibov.data.InSample$Date)

# Fitted list
# fit: Vetor do tipo lista que tera o fit para cada modelo listado
fit = vector(mode = "list", length = length(models))
names(fit) = names(models)

# Fit the sample: Para cada modelo faz a estimação "in sample"
for (i in 1:length(models)) {
  fit[[i]] = ugarchfit(data = r_ibov.xts, spec = models[[i]])
}

# Imprime os graficos dos fits.
for (i in 1:length(fit)) {
  modelSpec = rugarch::getspec(fit[[i]])
  
  g.subtitle = sprintf("Model: %s - Dist.: %s", modelSpec@model$modeldesc$vmodel,
                       modelSpec@model$modeldesc$distribution)
  
  p1 =   ggplot(Ibov.data.InSample, aes(x=Date)) +
    geom_line(aes(y = r_ibov), colour =  "grey") +
    geom_line(aes(y = sigma(fit[[i]])), colour =  "blue") +
    geom_line(aes(y = -sigma(fit[[i]])), colour =  "blue") +
    theme_bw() + 
    labs(title = "Bovespa Index volatility",
         subtitle = g.subtitle,
         caption = g.caption,
         y = NULL,
         x = NULL)
  
  
  ggplot2::ggsave(sprintf("Returns_%d_%s.png", i, names(fit[i])),
                  plot = p1,
                  device = "png", path = "./Trabalho/Plots/",
                  scale = 1.5, width = 6, height = 3, units = "in", dpi = 72)
  
}

#  remove componentes graficos que nao serao mais utilizados
rm(list = c("p1", "g.subtitle", "modelSpec"))


# Vetor com o nome de cada modelo
Models = c("ARCH Norm", "ARCH Sstd", "GARCH Norm", "GARCH Sstd", "E-GARCH Norm", "E-GARCH Sstd", 
           "Gjr-Garch Norm", "Gjr-Garch Sstd", "EWMA Norm", "EWMA Sstd", "apArch Norm", "apArch Sstd")

# Tabela de criterio de informacao
tb_InfoCriteria = tibble(Model = Models, fit = NA, Akaike = NA, Bayes = NA, Shibata=NA, HannanQuinn=NA)

for (i in 1:length(fit)) {
  tb_InfoCriteria[i, -c(1,2)] = t(rugarch::infocriteria(fit[[i]]))
  tb_InfoCriteria[i, 2] = names(fit[i])
}

# Imprime a tabela de criterio de informacão
print(tb_InfoCriteria)

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
cat(sprintf("Time frame: 2 years\nRefit every 5 days\n"))
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
cat(sprintf("Time frame: 2 years\nRefit every 20 days\n"))
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
cat(sprintf("Time frame: 2 years\nRefit every 60 days\n"))
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
cat(sprintf("Time frame: 2 years\nRefit every 252 days\n"))
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
cat(sprintf("Time frame: 5 years\nRefit every 5 days\n"))
fit.mov5_5 = vector(mode = "list", length = length(models))
names(fit.mov5_5) = names(models)
for (i in 1:length(models)) {
  cat(sprintf("Fitting: %s\n", names(fit.mov5_5[i])))
  fit.mov5_5[[i]] = ugarchroll(spec = models[[i]],
                               data = r_ibov.xts,
                               n.start = n.startForecast,
                               refit.window = "moving",
                               refit.every = 5)
}

# Expanding window recalculation every 20 days
cat(sprintf("Time frame: 5 years\nRefit every 20 days\n"))
fit.mov5_20 = vector(mode = "list", length = length(models))
names(fit.mov5_20) = names(models)
for (i in 1:length(models)) {
  cat(sprintf("Fitting: %s\n", names(fit.mov5_20[i])))
  fit.mov5_20[[i]] = ugarchroll(spec = models[[i]],
                                data = r_ibov.xts,
                                n.start = n.startForecast,
                                refit.window = "moving",
                                refit.every = 20)
}

# Expanding window recalculation every 60 days
cat(sprintf("Time frame: 5 years\nRefit every 60 days\n"))
fit.mov5_60 = vector(mode = "list", length = length(models))
names(fit.mov5_60) = names(models)
for (i in 1:length(models)) {
  cat(sprintf("Fitting: %s\n", names(fit.mov5_60[i])))
  fit.mov5_60[[i]] = ugarchroll(spec = models[[i]],
                                data = r_ibov.xts,
                                n.start = n.startForecast,
                                refit.window = "moving",
                                refit.every = 60)
}


# Expanding window recalculation every 252 days
cat(sprintf("Time frame: 5 years\nRefit every 252 days\n"))
fit.mov5_252 = vector(mode = "list", length = length(models))
names(fit.mov5_252) = names(models)
for (i in 1:length(models)) {
  cat(sprintf("Fitting: %s\n", names(fit.mov5_252[i])))
  fit.mov5_252[[i]] = ugarchroll(spec = models[[i]],
                                 data = r_ibov.xts,
                                 n.start = n.startForecast,
                                 refit.window = "moving",
                                 refit.every = 252)
}

# Save all models forecast
save(fit.roll_5,
     fit.roll_20,
     fit.roll_60,
     fit.roll_252,
     fit.mov2_5,
     fit.mov2_20,
     fit.mov2_60,
     fit.mov2_252,
     fit.mov5_5,
     fit.mov5_20,
     fit.mov5_60,
     fit.mov5_252,
     file = "./Trabalho/Database/GarchForecast.RData")

