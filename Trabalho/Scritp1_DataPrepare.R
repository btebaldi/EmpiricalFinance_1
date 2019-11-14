# Limpeza de variaveis
rm(list = ls())

# Carrega bibliotecas utilizadas
library(readr)
library(dplyr)

# Carrega base de dados
data <- read_csv("Trabalho/Database/IBOV.csv", col_types = cols(AdjClose = col_double(), 
                                                       Close = col_double(), Date = col_date(format = "%Y-%m-%d"), 
                                                       High = col_double(), Low = col_double(), 
                                                       Open = col_double(), Volume = col_double()), na = c("null", "NA"))

# filtra dados para o periodo de 2000-01-01 a 2019-01-01
Ibov.data = data %>% dplyr::filter(Date >= "2000-01-01", Date <= "2019-01-01")

cat(sprintf("Minimum date: %s\nMaximum date: %s", min(Ibov.data$Date), max(Ibov.data$Date)))

# Salva dados
save(Ibov.data, file = "./Trabalho/Database/Ibovespa.RData")
