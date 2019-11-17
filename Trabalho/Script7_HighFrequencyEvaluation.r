# Limpeza de variaveis e graficos antigos
rm(list = ls())

library(dplyr)
library(forecast)
library(zoo)

# Load all models forecast
load("./Trabalho/Database/Garch_HF_Forecast.RData")


# Lista todos os modelos que serão comparados
variableNames = c("fit.roll_5", "fit.roll_20", "fit.roll_60",
                  "fit.mov3_5", "fit.mov3_20", "fit.mov3_60")

# Inicializa tabela que conterá resultados 
tbl = NULL
for(str in variableNames){
  
  # busca o modelo na lista de variaveis
  mdl = get(str)
  
  for (i in 1:length(mdl)) {
    
    # dist = unlist(strsplit(names(mdl[i]), "\\."))[2]
    # 
    # if(dist == "sstd")
    # {cat(sprintf("%s\n",names(mdl[i])))}
    
    # Get the predicitons and realized values.
    result = tryCatch({
      preds <- as.data.frame(mdl[[i]])
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
      tb_aux = tibble(Id = 1:nrow(preds), Date=NA, Model=NA, Dist=NA,  e= NA, q=NA)
      tb_aux$Date = rownames(preds)
      
      # Determina uma coluna de data nas previsoes
      preds$date = zoo::as.Date(row.names(preds))
      preds = dplyr::inner_join(preds, Ibov.data, by = c("date" = "Date"))

      # Prediction error for the variance
      d <- preds$VR - preds$Sigma^2
      
      q <- preds$VR/preds$Sigma^2 - log(preds$VR/preds$Sigma^2)
      
      # Store prediction error in table
      tb_aux$e = d^2
      tb_aux$q = q
    }
    else{
      tb_aux = tibble(Id = 1, Date=NA, Model=NA, Dist=NA,  e= NA, q=NA)
      tb_aux$Date = NA
      
      
    }
    
    tb_aux$Model = unlist(strsplit(names(mdl[i]), "\\."))[1]
    tb_aux$Dist = unlist(strsplit(names(mdl[i]), "\\."))[2]
    tb_aux$Estimation = str
    # append the table
    tbl = rbind(tbl, tb_aux)
  }  
  
}
rm(list = variableNames)
rm(list = c("tb_aux", "d", "q", "i", "str", "variableNames", "Ibov.data", "mdl", "preds", "models"))


tbl$e = tbl$e * 1e7

# Calcula o MSE dos modelos
tbl_temp = tbl %>% dplyr::filter(Estimation == "fit.roll_5") %>% 
  dplyr::group_by(Model, Dist) %>% summarise(MSE = mean(e)) %>% 
  tidyr::pivot_wider(id_cols=c("Model"),
                     names_from = "Dist",
                     names_prefix = "",
                     names_repair = "check_unique",
                     values_from = MSE)

# Calcula os DM
# Inicializa a coluna
tbl_temp$dm =NA
models = c("arch", "eGarch", "EWMA", "garch")
for (mdl in models) {
  series.norm = tbl %>% dplyr::filter(Estimation == "fit.roll_5", Model==mdl, Dist=="norm")
  series.sstd = tbl %>% dplyr::filter(Estimation == "fit.roll_5", Model==mdl, Dist=="sstd")
  # plot(series.norm$e,series.sstd$e)
  dmtest = forecast::dm.test(e1=series.norm$e, e2=series.sstd$e)
  tbl_temp[tbl_temp$Model==mdl,"dm"] = dmtest$p.value
}
rm(list=c("models", "dmtest", "series.norm", "series.sstd"))

print(tbl_temp)
write.table(tbl_temp , file = "./Trabalho/Tables/Table1_HF_MSE.csv")

# Calcula o QL dos modelos
tbl_temp = tbl %>% dplyr::filter(Estimation == "fit.roll_5") %>% 
  dplyr::group_by(Model, Dist) %>% summarise(QL = mean(q)) %>% 
  tidyr::pivot_wider(id_cols=c("Model"),
                     names_from = "Dist",
                     names_prefix = "",
                     names_repair = "check_unique",
                     values_from = QL)

tbl_temp$dm =NA
models = c("arch", "eGarch", "EWMA", "garch")
for (mdl in models) {
  series.norm = tbl %>% dplyr::filter(Estimation == "fit.roll_5", Model==mdl, Dist=="norm")
  series.sstd = tbl %>% dplyr::filter(Estimation == "fit.roll_5", Model==mdl, Dist=="sstd")
  # plot(series.norm$q,series.sstd$q)
  dmtest = forecast::dm.test(e1=series.norm$e, e2=series.sstd$e)
  tbl_temp[tbl_temp$Model==mdl,"dm"] = dmtest$p.value
}
rm(list=c("models", "dmtest", "series.norm", "series.sstd"))

print(tbl_temp)
write.table(tbl_temp , file = "./Trabalho/Tables/Table1_HF_QL.csv")

# Comparacao de moving vs roling
tbl_temp = tbl %>% dplyr::filter(Dist == "norm") %>% 
  dplyr::group_by(Model, Dist, Estimation) %>% summarise(MSE = mean(e)) %>% 
  tidyr::pivot_wider(id_cols=c("Model", "Dist"),
                     names_from = "Estimation",
                     names_prefix = "",
                     names_repair = "check_unique",
                     values_from = MSE)

print(tbl_temp)
write.table(tbl_temp , file = "./Trabalho/Tables/Table1_HF_MSE_Overall.csv")

tbl_temp = tbl %>% dplyr::filter(Dist == "norm") %>% 
  dplyr::group_by(Model, Dist, Estimation) %>% summarise(QL = mean(q)) %>% 
  tidyr::pivot_wider(id_cols=c("Model", "Dist"),
                     names_from = "Estimation",
                     names_prefix = "",
                     names_repair = "check_unique",
                     values_from = QL)

print(tbl_temp)
write.table(tbl_temp , file = "./Trabalho/Tables/Table1_HF_QL_Overall.csv")

