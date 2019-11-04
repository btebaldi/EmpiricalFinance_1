# Limpeza de variaveis e graficos antigos
rm(list = ls())

# Bibliotecas utilizadas
library(dplyr)
library(xts)
library(dlm)
library(ggplot2)
# library(quantmod)
library(ggExtra)
library(cowplot)

# carrega banco de dados
load("./Ibovespa.RData")

summary(Ibov.data)

# transforma em objeto xts (desnecessario???)
# Ibov.xts <- xts(Ibov.data[,-1], order.by=Ibov.data$Date)

# Ordena a base de dados (just in case)
Ibov.data = Ibov.data %>% dplyr::arrange(Date) 

# Cria uma funcao que cria um polinomio de Dynamic Linear Model (DLM) de ordem 1.
# utiliza exp nas variancias para garantir que serao sempre maior que zero.
fn <- function(params){
  # Create an n-th order polynomial DLM
  dlm::dlmModPoly(order= 1, dV = exp(params[1]) , dW = exp(params[2]))
}

# Determina qual a seria que sera utilizada para o local level model
y <- Ibov.data$AdjClose
  
# chute iniicial da variancia
a0 <- log(var(y, na.rm = T))

# Estima os parametros do modelo de nível local
# Recall: local level deve ser especificado duas varaincias
fit <- dlm::dlmMLE(y, c(a0,0), fn)

# The local level model
mod <- fn(fit$par)
  
obs.error.var <- V(mod)
# [,1]
# [1,] 0.003268212
state.error.var <- W(mod)
# [,1]
# [1,] 0.004703001

# Applies Kalman filter to compute filtered values of the state vectors,
# together with their variance/covariance matrices
filtered <- dlmFilter(y, mod)

# Apply Kalman smoother to compute smoothed values of the state vectors, 
# together with their variance/covariance matrices.
smoothed <- dlmSmooth(filtered)

# Obtem a serie de mu
mu <- dropFirst(smoothed$s)

# res <- residuals(filtered,sd=F)

# Adiciona serie de nivel local a base de dados
Ibov.data$mu = mu

# Preenche os dados faltantes
Ibov.data[is.na(Ibov.data$Close), "AdjClose"] = mu[is.na(Ibov.data$Close)]

g.caption = sprintf("Bovespa Index from %s, to %s", min(Ibov.data$Date), max(Ibov.data$Date))

ggplot(Ibov.data, aes(x = Date)) + 
  geom_line(aes(y=AdjClose)) + 
  # geom_line(aes(y=Volume), colour = "Red")
  labs(title = "Bovespa Index",
       caption = g.caption,
       y = "Close",
       x = NULL)

# p2 <- ggplot(Ibov.data, aes(Date, Volume)) + 
#   geom_line(colour = "Red")
# 
# 
# plot_grid(p1, p2, label_size = 12, nrow = 2, ncol = 1)

save(Ibov.data, file = "Ibovespa_SemBuracao.RData")
