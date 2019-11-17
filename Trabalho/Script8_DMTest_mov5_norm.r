# Limpeza de variaveis e graficos antigos
rm(list = ls())

library(dplyr)
library(forecast)
library(zoo)

# Load all models forecast
load("./Trabalho/Database/GarchForecast.RData")

# Remove modelos que nao serao analisados
rm(list = c("fit.roll_5", "fit.roll_20", "fit.roll_60", "fit.roll_252",
            "fit.mov2_5", "fit.mov2_20", "fit.mov2_60", "fit.mov2_252",
            "fit.mov5_20", "fit.mov5_60", "fit.mov5_252"))
            
  # busca o modelo na lista de variaveis
  mdl = fit.mov5_5
  
  tbl = NULL
  
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
      
      # Prediction error for the mean
      e <- preds$Realized - preds$Mu
      
      # Prediction error for the variance
      d <- e^2 - preds$Sigma^2
      
      q <- e^2/preds$Sigma^2 - log(e^2/preds$Sigma^2)
      
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
    
    # append the table
    tbl = rbind(tbl, tb_aux)
  }  
  

# rm(list = variableNames)
rm(list = c("tb_aux", "d", "q", "i", "e", "result", "mdl", "preds"))


tbl$e = tbl$e * 1e7

# Calcula o MSE dos modelos


tbl_temp = tbl %>% dplyr::filter(Dist == "norm")

models = tbl_temp %>% dplyr::distinct(Model)

DM = matrix(data = NA, nrow = nrow(models), ncol = nrow(models))
colnames(DM) = models$Model
rownames(DM) = models$Model

for (i in models$Model) {
  for (j in models$Model) {
    series1 = tbl_temp %>% dplyr::filter(Model==i)
    series2 = tbl_temp %>% dplyr::filter(Model==j)
    
    # cat(sprintf("%s  %f\n", i, mean(series1$e)))
    # cat(sprintf("%s - %f\n", j, mean(series2$e)))
    if(i!=j){
    dmtest = forecast::dm.test(e1=series1$e, e2=series2$e, alternative = "less")
    DM[i,j] = dmtest$p.value    
    }
  }
  
}

print(DM)
write.table(DM , file = "./Trabalho/Tables/Table6_DM_MSE.csv")
