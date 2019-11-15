# Limpeza de variaveis e graficos antigos
rm(list = ls())

# Bibliotecas utilizadas
library(tibble)
library(dplyr)
library(tidyr)
# library(xts)
# library(ggplot2)
# library(cowplot)
# library(TSA)
library(rugarch)

# source("./Trabalho/ggplot_Acf_Pacf.R")

# Load data
load("./GitHub/EmpiricalFinance_1/Trabalho/Database/GarchFrecast.RData")

# Declara uma funcao para extrair e calcular os erros de previsao
GetPredError = function(fit.model){
  
  # table with errors
  tb_PredError = tibble(Model = 1:length(fit.model),  fit = NA, MSE = NA, QL = NA)
  
  # cylce models
  for (i in 1:length(fit.model)) {
    cat(sprintf("Calculating prediction error for: %s\n", names(fit.model[i])))
    
    # Get the predicitons and realized values.
    result = tryCatch({
      preds <- as.data.frame(fit.model[[i]])
      TRUE
    }, warning = function(w) {
      print(paste("MY_WARNING:  ",war))
      
      return(TRUE)
    }, error = function(e) {
      print(paste("MY_ERROR:  ", e))
      
      return(FALSE)
    }, finally = {
      
    })
    
    
    if (result){
      # class(fit.model[[i]])
      # Prediction error for the mean
      e <- preds$Realized - preds$Mu
      
      # Prediction error for the variance
      d <- e^2 - preds$Sigma^2
      
      q <- e^2/preds$Sigma^2 - log(e^2/preds$Sigma^2)
      
      # Store prediction error in table
      tb_PredError[i, "MSE"] = mean(d^2)
      tb_PredError[i, "QL"] = mean(q)
    }
    tb_PredError[i, "fit"] = names(fit.mov5_252[i])
  }
  
  # Return table
  return(tb_PredError)
}

# Lista todos os modelos que serão comparados
variableNames = c("fit.roll_5", "fit.roll_20", "fit.roll_60", "fit.roll_252",
                  "fit.mov2_5", "fit.mov2_20", "fit.mov2_60", "fit.mov2_252",
                  "fit.mov5_5", "fit.mov5_20", "fit.mov5_60", "fit.mov5_252")

# Calcula o erro de previsao para todos os modelos e aagrupa os dados em uma tabela
tbl = NULL
for (str in variableNames) {
  # busca a variavel no environment
  mdl = get(str)
  cat(sprintf("Model: %s\n", str))
  tbl_2 = GetPredError(mdl)
  tbl_2$ModelName = str
  
  tbl = rbind(tbl, tbl_2)
}

rm(list = )

tidyr::pivot_wider(tbl)

# Avaliacao de mudanca dos estimadores no tempo (fazer para o melhor previsor)
