# Limpeza de variaveis e graficos antigos
rm(list = ls())
graphics.off()

# Carrega bibliotecas utilizadas
library(readr)
library(dplyr)

# Carrega base de dados
data <- read_csv("database/IBOV.csv", col_types = cols(AdjClose = col_double(), 
                                                       Close = col_double(), Date = col_date(format = "%Y-%m-%d"), 
                                                       High = col_double(), Low = col_double(), 
                                                       Open = col_double(), Volume = col_double()), na = c("null", "NA"))


Ibov.data = data %>% dplyr::filter(Date >= "2000-01-01", Date <= "2019-01-01")

cat(sprintf("Minimum date: %s\nMaximum date: %s", min(Ibov.data$Date), max(Ibov.data$Date)))

save(Ibov.data, file = "Ibovespa.RData")
