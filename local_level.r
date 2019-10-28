# Limpeza de variaveis e graficos antigos
rm(list = ls())
graphics.off()

# Bibliotecas utilizadas
library(dplyr)
library(xts)
library(dlm)
library(ggplot2)

# carrega banco de dados
load("./Ibovespa.RData")

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
y <- d$Close
  
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
d$mu = mu

# Preenche os dados faltantes
d[is.na(d$Close), "AdjClose"]

ggplot(d, aes(x=1:nrow(d))) +
  geom_line(aes(y=Open), colour= "red") +
  geom_line(aes(y=mu), colour= "black")




d1 = y
d1[is.na(dados3_2018$IBOV )] = mu[is.na(dados3_2018$IBOV)]
write.table(d1, "myIBOV.txt", sep="\t")



par(mfrow=c(1,1))
temp <- window(cbind(logNorFatalities.ts,mu))
plot(temp , plot.type="single" , col =c("black","blue"),lty=c(1,2),
     xlab="",ylab = "log KSI")
legend("topright",leg = c("log UK drivers KSI"," stochastic level"),
       cex = 0.7, lty = c(1, 2),
       col = c("darkgrey","blue"),pch=c(3,NA),bty = "y", horiz = T)


