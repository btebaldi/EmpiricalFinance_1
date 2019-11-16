# Limpeza de variaveis e graficos antigos
rm(list = ls())


# Bibliotecas utilizadas
library(tibble)
library(dplyr)
library(tidyr)
library(zoo)
library(rugarch)
# library(lubridate)

# Carega base de dados de High Frequency e modelos
load("./Trabalho/Database/Ibovespa_HighFreq.RData")
load("./Trabalho/Database/GarchModels.RData")

# Seleciona apenas as colunas de retorno e data
Ibov.data = Ibov.data %>% dplyr::select(Date, r_ibov)
Ibov.HF_data.Wide = Ibov.HF_data.Wide %>% dplyr::select(Data, VR, VOLR)

Ibov.data = dplyr::inner_join(Ibov.data, Ibov.HF_data.Wide, by = c("Date" = "Data"))
rm(list = c("Ibov.HF_data.Wide"))

summary(Ibov.data)


# *************** MOVING WINDOW (3 year) ***************
# 2003-01-01 = 738
n.startForecast = nrow(Ibov.data %>% dplyr::filter(Date < "2003-01-01")) + 1

# Reconstroi a serie XTS, pois agora vamos usar roling window forecast
r_ibov.xts = xts::xts(Ibov.data$r_ibov, order.by = Ibov.data$Date)

# Determina qual o periodo de reestimacao
# Expanding window recalculation every 5 days
cat(sprintf("Time frame: 3 years\nRefit every 5 days\n"))
fit.roll_5 = vector(mode = "list", length = length(models))
names(fit.roll_5) = names(models)
for (i in 1:length(models)) {
  cat(sprintf("Fitting: %s\n", names(fit.roll_5[i])))
  fit.roll_5[[i]] = ugarchroll(spec = models[[i]],
                               data = r_ibov.xts,
                               n.start = n.startForecast,
                               refit.window = "recursive",
                               refit.every = 5)
}

# Expanding window recalculation every 20 days
cat(sprintf("Time frame: 3 years\nRefit every 20 days\n"))
fit.roll_20 = vector(mode = "list", length = length(models))
names(fit.roll_20) = names(models)
for (i in 1:length(models)) {
  cat(sprintf("Fitting: %s\n", names(fit.roll_20[i])))
  fit.roll_20[[i]] = ugarchroll(spec = models[[i]],
                                data = r_ibov.xts,
                                n.start = n.startForecast,
                                refit.window = "recursive",
                                refit.every = 20)
}

# Expanding window recalculation every 60 days
cat(sprintf("Time frame: 3 years\nRefit every 60 days\n"))
fit.roll_60 = vector(mode = "list", length = length(models))
names(fit.roll_60) = names(models)
for (i in 1:length(models)) {
  cat(sprintf("Fitting: %s\n", names(fit.roll_60[i])))
  fit.roll_60[[i]] = ugarchroll(spec = models[[i]],
                                data = r_ibov.xts,
                                n.start = n.startForecast,
                                refit.window = "recursive",
                                refit.every = 60)
}



# *************** MOVING WINDOW (3 year) ***************
# Determina qual o periodo de reestimacao
# Expanding window recalculation every 5 days
cat(sprintf("Time frame: 3 years\nRefit every 5 days\n"))
fit.mov3_5 = vector(mode = "list", length = length(models))
names(fit.mov3_5) = names(models)
for (i in 1:length(models)) {
  cat(sprintf("Fitting: %s\n", names(fit.mov3_5[i])))
  fit.mov3_5[[i]] = ugarchroll(spec = models[[i]],
                               data = r_ibov.xts,
                               n.start = n.startForecast,
                               refit.window = "moving",
                               refit.every = 5)
}

# Expanding window recalculation every 20 days
cat(sprintf("Time frame: 3 years\nRefit every 20 days\n"))
fit.mov3_20 = vector(mode = "list", length = length(models))
names(fit.mov3_20) = names(models)
for (i in 1:length(models)) {
  cat(sprintf("Fitting: %s\n", names(fit.mov3_20[i])))
  fit.mov3_20[[i]] = ugarchroll(spec = models[[i]],
                                data = r_ibov.xts,
                                n.start = n.startForecast,
                                refit.window = "moving",
                                refit.every = 20)
}

# Expanding window recalculation every 60 days
cat(sprintf("Time frame: 3 years\nRefit every 60 days\n"))
fit.mov3_60 = vector(mode = "list", length = length(models))
names(fit.mov3_60) = names(models)
for (i in 1:length(models)) {
  cat(sprintf("Fitting: %s\n", names(fit.mov3_60[i])))
  fit.mov3_60[[i]] = ugarchroll(spec = models[[i]],
                                data = r_ibov.xts,
                                n.start = n.startForecast,
                                refit.window = "moving",
                                refit.every = 60)
}


# Save all models forecast
save(fit.roll_5,
     fit.roll_20,
     fit.roll_60,
     fit.mov3_5,
     fit.mov3_20,
     fit.mov3_60,
     Ibov.data,
     models,
     file = "./Trabalho/Database/Garch_HF_Forecast.RData")
