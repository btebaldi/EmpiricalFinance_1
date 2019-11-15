# Limpeza de variaveis e graficos antigos
rm(list = ls())

# Bibliotecas utilizadas
library(tibble)
library(dplyr)
library(tidyr)
library(rugarch)

# Load data
load("./Trabalho/Database/GarchForecast.RData")

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
    tb_PredError[i, "fit"] = names(fit.model[i])
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
  str_split = strsplit(str, "_")
  tbl_2$EstimationName = str_split[[1]][1]
  tbl_2$Restimation = as.numeric(str_split[[1]][2])
  tbl = rbind(tbl, tbl_2)
}
rm(list = c("str", "tbl_2", "mdl", "str_split"))

tbl$MSE = tbl$MSE * 1e7

for (i in 1:nrow(tbl)) {
  tbl[i,c("ModelName", "Distribution")] = unlist(strsplit(tbl[[i, "fit"]], "\\."))    
}


# Comparacao de Normal vs t-student in fit.roll
tbl_temp = tbl %>% dplyr::filter(EstimationName == "fit.roll") %>% tidyr::pivot_wider(id_cols=c("fit"),
                                                                      names_from = "Restimation",
                                                                      names_prefix = "Rest_",
                                                                      names_repair = "check_unique",
                                                                      values_from = MSE)
write.table(tbl_temp , file = "./Trabalho/Tables/Table1_MSE.csv")
print(tbl_temp)

tbl_temp = tbl %>% dplyr::filter(EstimationName == "fit.roll") %>% tidyr::pivot_wider(id_cols=c("fit"),
                                                                      names_from = "Restimation",
                                                                      names_prefix = "Rest_",
                                                                      names_repair = "check_unique",
                                                                      values_from = QL)

write.table(tbl_temp , file = "./Trabalho/Tables/Table1_QL.csv")
print(tbl_temp)

# Comparação do metodo expanding vs moving usando Normal
tbl_temp = tbl %>% dplyr::filter(Distribution == "norm", Restimation ==5) %>% tidyr::pivot_wider(id_cols=c("ModelName"),
                                                                     names_from = "EstimationName",
                                                                     names_prefix = NULL,
                                                                     names_repair = "check_unique",
                                                                     values_from = MSE)

write.table(tbl_temp , file = "./Trabalho/Tables/Table2_MSE.csv")
print(tbl_temp)

tbl_temp = tbl %>% dplyr::filter(Distribution == "norm", Restimation ==5) %>% tidyr::pivot_wider(id_cols=c("ModelName"),
                                                                                                 names_from = "EstimationName",
                                                                                                 names_prefix = NULL,
                                                                                                 names_repair = "check_unique",
                                                                                                 values_from = QL)
write.table(tbl_temp , file = "./Trabalho/Tables/Table2_QL.csv")
print(tbl_temp)

# Comparação do metodo expanding vs moving usando t-Student
tbl_temp = tbl %>% dplyr::filter(Distribution == "sstd", Restimation ==5) %>% tidyr::pivot_wider(id_cols=c("ModelName"),
                                                                                                 names_from = "EstimationName",
                                                                                                 names_prefix = NULL,
                                                                                                 names_repair = "check_unique",
                                                                                                 values_from = MSE)
write.table(tbl_temp , file = "./Trabalho/Tables/Table2_MSE_sstd.csv")
print(tbl_temp)

tbl_temp = tbl %>% dplyr::filter(Distribution == "sstd", Restimation ==5) %>% tidyr::pivot_wider(id_cols=c("ModelName"),
                                                                                                 names_from = "EstimationName",
                                                                                                 names_prefix = NULL,
                                                                                                 names_repair = "check_unique",
                                                                                                 values_from = QL)
write.table(tbl_temp , file = "./Trabalho/Tables/Table2_QL_sstd.csv")
print(tbl_temp)

# Salva a tabela completa em CSV
write.table(tbl , file = "./Trabalho/Tables/Table3_overall.csv")
print(tbl)
