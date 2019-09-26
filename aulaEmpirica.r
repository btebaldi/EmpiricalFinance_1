# limpeza de variaveis antigas
rm(list=ls())

# Carrega bibliotecas utilizadas
library(readxl)

# Carrega banco de dados
Dados_Q1 <- read_excel("Dados_HFDATA.xls", 
                       col_names = c("Date","SP500", "Rt", "Rt2", "sigma"),
                       sheet = "Q.1", range = "a3:e2516")
head(Dados_Q1)

Dados_Q2 <- read_excel("Dados_HFDATA.xls", 
                       col_names = c("Date","AdjClose", "High", "Low", "Dt", "RPt", "sigma"),
                       sheet = "Q2", range = "a3:g2516")
head(Dados_Q2)




