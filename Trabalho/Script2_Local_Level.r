# Limpeza de variaveis e graficos antigos
rm(list = ls())

# Bibliotecas utilizadas
library(dplyr)
library(dlm)
library(ggplot2)

# carrega banco de dados
load("./Trabalho/Database/Ibovespa.RData")

# Estatisticas descritivas
summary(Ibov.data)

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
state.error.var <- W(mod)

cat(sprintf("Observation error variance: %f\nState error variance: %f", obs.error.var, state.error.var))

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


# Imprime grafico da serie 
g.caption = "Source: Yahoo finance"

p1 = ggplot(Ibov.data, aes(x = Date)) + 
  geom_line(aes(y=AdjClose)) + 
  labs(title = "Bovespa Index",
       caption = g.caption,
       y = NULL,
       x = NULL)

ggsave("BovespaIndex_Level.png", plot = p1, device = "png", path = "./Trabalho/Plots/",
       scale = 2, width = 6, height = 3, units = "in",
       dpi = 72)

# Salva os dados
save(Ibov.data, file = "./Trabalho/Database/Ibovespa_SemBuracao.RData")
