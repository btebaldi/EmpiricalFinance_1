# Limpeza de variaveis e graficos antigos
rm(list = ls())

# Bibliotecas utilizadas
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

source("./ggplot_Acf_Pacf.R")

# carrega banco de dados
Ibov.HF_data <- read_excel("Trabalho/IBOVESPA15m.xlsx")
colnames(Ibov.HF_data) = c("Data", "Hora", "Ano", "Mes", "Dia", "DiaSemana", "SeqHora", "Ibov"  )
head(Ibov.HF_data)

# Organiza a base de dados em ordem cronologica.
Ibov.HF_data = Ibov.HF_data %>% dplyr::arrange(Ano, Mes, Dia, SeqHora)

# Estatisticas descritivas
summary(Ibov.HF_data)

# Passa o log no indice e calcula o retorno
Ibov.HF_data$ln_Ibov = log(Ibov.HF_data$Ibov)
Ibov.HF_data$r_Ibov = Ibov.HF_data$ln_Ibov - dplyr::lag(Ibov.HF_data$ln_Ibov)

# Calcula o retorno ao quadrado
Ibov.HF_data$r_sq_Ibov = Ibov.HF_data$r_Ibov^2

plots = ggplot_Acf_Pacf(Ibov.HF_data$r_sq_Ibov[-1], 200)

plots$ACF + 
  labs(title = "Bovespa Index",
       subtitle = "Quadrado dos retornos intradiários",
       caption = "Source: EESP-FGV",
       y = "ACF",
       x = NULL)

plots$PACF + 
  labs(title = "Bovespa Index",
       subtitle = "Quadrado dos retornos intradiários",
       caption = "Source: EESP-FGV",
       y = "PACF",
       x = NULL)

Ibov.HF_data.Wide = tidyr::pivot_wider(Ibov.HF_data,
                         id_cols = c("Ano", "Mes", "Dia", "DiaSemana", "SeqHora"),
                         names_from = SeqHora,
                         names_prefix = "Hora_",
                         names_repair = "check_unique",
                         values_from = r_sq_Ibov)

# Calcula a Variancia Realizada
Ibov.HF_data.Wide$VR = rowSums(Ibov.HF_data.Wide[,paste("Hora_", 1:29, sep = "")], na.rm = TRUE)

# Calcula a Volatilidade Realizada
Ibov.HF_data.Wide$VOLR = Ibov.HF_data.Wide$VR^0.5

# plota o grafico da volatilidade realizada
ggplot(Ibov.HF_data.Wide, aes(x = 1:nrow(Ibov.HF_data.Wide))) + 
  geom_line(aes(y=VOLR)) + 
  labs(title = "Bovespa Index",
       subtitle = "Volatilidade realizada",
       caption = "Source: EESP-FGV",
       y = NULL,
       x=NULL)

# Salva os dados
save(Ibov.HF_data.Wide, file = "Ibovespa_HighFreq.RData")
