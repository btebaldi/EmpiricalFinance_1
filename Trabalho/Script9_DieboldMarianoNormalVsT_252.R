# Limpeza de variaveis e graficos antigos
rm(list = ls())

# Bibliotecas utilizadas
library(tibble)
library(dplyr)
library(tidyr)
library(rugarch)
library(forecast)

# Load data
load("./Trabalho/Database/GarchForecast.RData")

cat(sprintf("Script que realiza o teste de Diebold Mariano para t-Student vs Normal para Expandind window 252"))

tbl = NULL

for (i in 1:length(fit.roll_252)) {
  # Get the predicitons and realized values.
  preds <- as.data.frame(fit.roll_252[[i]])
  
  tb_aux = tibble(Id = 1:nrow(preds), Date=NA, Model=NA, Dist=NA,  e= NA, q=NA)
  tb_aux$Date = rownames(preds)    
  
  tb_aux$Model = unlist(strsplit(names(fit.roll_252[i]), "\\."))[1]
  tb_aux$Dist = unlist(strsplit(names(fit.roll_252[i]), "\\."))[2]
  # Prediction error for the mean
  e <- preds$Realized - preds$Mu
  
  # Prediction error for the variance
  d <- e^2 - preds$Sigma^2
  
  q <- e^2/preds$Sigma^2 - log(e^2/preds$Sigma^2)
  
  # Store prediction error in table
  tb_aux$e = d^2
  tb_aux$q = q
  
  # append the table
  tbl = rbind(tbl, tb_aux)
}



tbl$e = tbl$e * 1e7


# Comparacao de Normal vs t-student in fit.roll 252
tbl_temp = tbl %>% 
  tidyr::pivot_wider(id_cols=c("Model", "Date"),
                     names_from = "Dist",
                     names_prefix = "",
                     names_repair = "check_unique",
                     values_from = e)

tbl_temp %>% dplyr::distinct(Model)

# This function implements the modified test proposed by Harvey, Leybourne and Newbold (1997). 
# The null hypothesis is that the two methods have the same forecast accuracy. 
# For alternative="less", the alternative hypothesis is that method 2 is less accurate than method 1.
# For alternative="greater", the alternative hypothesis is that method 2 is more accurate than method 1.
# For alternative="two.sided", the alternative hypothesis is that method 1 and method 2 have different 
# levels of accuracy.
  
tbl_temp2 = tbl_temp %>% dplyr::filter(Model == "arch")
forecast::dm.test(e1 = tbl_temp2$norm, e2 = tbl_temp2$sstd)

tbl_temp2 = tbl_temp %>% dplyr::filter(Model == "garch")
forecast::dm.test(e1 = tbl_temp2$norm, e2 = tbl_temp2$sstd)

tbl_temp2 = tbl_temp %>% dplyr::filter(Model == "eGarch")
forecast::dm.test(e1 = tbl_temp2$norm, e2 = tbl_temp2$sstd)

tbl_temp2 = tbl_temp %>% dplyr::filter(Model == "gjrGarch")
forecast::dm.test(e1 = tbl_temp2$norm, e2 = tbl_temp2$sstd)

tbl_temp2 = tbl_temp %>% dplyr::filter(Model == "EWMA")
forecast::dm.test(e1 = tbl_temp2$norm, e2 = tbl_temp2$sstd)

tbl_temp2 = tbl_temp %>% dplyr::filter(Model == "apArch")
forecast::dm.test(e1 = tbl_temp2$norm, e2 = tbl_temp2$sstd)


tbl_temp = tbl %>% 
  tidyr::pivot_wider(id_cols=c("Model", "Date"),
                     names_from = "Dist",
                     names_prefix = "",
                     names_repair = "check_unique",
                     values_from = q)

tbl_temp2 = tbl_temp %>% dplyr::filter(Model == "arch")
forecast::dm.test(e1 = tbl_temp2$norm, e2 = tbl_temp2$sstd)

tbl_temp2 = tbl_temp %>% dplyr::filter(Model == "garch")
forecast::dm.test(e1 = tbl_temp2$norm, e2 = tbl_temp2$sstd)

tbl_temp2 = tbl_temp %>% dplyr::filter(Model == "eGarch")
forecast::dm.test(e1 = tbl_temp2$norm, e2 = tbl_temp2$sstd)

tbl_temp2 = tbl_temp %>% dplyr::filter(Model == "gjrGarch")
forecast::dm.test(e1 = tbl_temp2$norm, e2 = tbl_temp2$sstd)

tbl_temp2 = tbl_temp %>% dplyr::filter(Model == "EWMA")
forecast::dm.test(e1 = tbl_temp2$norm, e2 = tbl_temp2$sstd)

tbl_temp2 = tbl_temp %>% dplyr::filter(Model == "apArch")
forecast::dm.test(e1 = tbl_temp2$norm, e2 = tbl_temp2$sstd)
