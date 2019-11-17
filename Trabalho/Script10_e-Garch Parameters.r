# Limpeza de variaveis e graficos antigos
rm(list = ls())

library(dplyr)
library(tidyr)
library(forecast)
library(zoo)
library(ggplot2)

# Load all models forecast
load("./Trabalho/Database/GarchForecast.RData")
load("./Trabalho/Database/Ibovespa_SemBuracao.RData")

# Seleiona o vetor de datas
x = Ibov.data %>% dplyr::select(Date, AdjClose)
x$r = log(x$AdjClose) - log(dplyr::lag(x$AdjClose)) 
x = x[-1,]
x$r2 = x$r^2
# x= x %>% dplyr::filter(Date >= "2013-01-01") %>% ggplot()+ geom_line(aes(Date, r2))
  
rm(list = c("Ibov.data"))


# Remove modelos que nao serao analisados
rm(list = c("fit.roll_5", "fit.roll_20", "fit.roll_60", "fit.roll_252",
            "fit.mov2_5", "fit.mov2_20", "fit.mov2_60", "fit.mov2_252",
            "fit.mov5_20", "fit.mov5_60", "fit.mov5_252"))

# busca o modelo na lista de variaveis
mdl = fit.mov5_5[[5]]

list.coef = rugarch::coef(mdl)


tbl = tibble(Date=zoo::as.Date("1999-01-01"), Coef = rep(c("mu", "omega", "alpha1", "beta1", "gamma1"), length(list.coef)),
              Estimate = NA, StdError = NA, t_value = NA,  pValue = NA)
for (i in 1:length(list.coef)) {
  cur_coef = list.coef[[i]]
  selector = 
  for (j in 1:5) {
    tbl[(j+(i-1)*5), "Date"] = zoo::as.Date(cur_coef$index)    
    tbl[(j+(i-1)*5), c("Estimate", "StdError", "t_value","pValue")] = cur_coef$coef[j,]
  }
}

tbl_2 = 
  tbl %>% tidyr::pivot_wider(id_cols = c("Date"), 
                           names_from = "Coef",
                           values_from = "Estimate") %>% 
  full_join(x, by = c("Date" = "Date")) %>%
  dplyr::filter(Date >= "2013-01-01") %>% 
  dplyr::arrange(Date)

tbl_2$mu = na.locf(tbl_2$mu)
tbl_2$omega = na.locf(tbl_2$omega)
tbl_2$alpha1 = na.locf(tbl_2$alpha1)
tbl_2$beta1  = na.locf(tbl_2$beta1)
tbl_2$gamma1 = na.locf(tbl_2$gamma1)


tbl_2$mu = tbl_2$mu / tbl_2$mu[1]
tbl_2$omega = tbl_2$omega / tbl_2$omega[1]
tbl_2$alpha1 = tbl_2$alpha1 / tbl_2$alpha1[1]
tbl_2$beta1  = tbl_2$beta1 / tbl_2$beta1[1]
tbl_2$gamma1 = tbl_2$gamma1 / tbl_2$gamma1[1]


g1 = ggplot(tbl_2) +
  geom_line(aes(x=Date, y=mu, colour="mu")) +
  geom_line(aes(x=Date, y=omega, colour="omega")) +
  geom_line(aes(x=Date, y=alpha1, colour="alpha")) +
  geom_line(aes(x=Date, y=beta1, colour="beta")) +
  geom_line(aes(x=Date, y=gamma1, colour="gamma")) +
  scale_colour_manual(NULL,breaks = c("mu", "omega", "alpha", "beta", "gamma"),
                      values = c("black", "red", "green", "blue", "coral")) +
  theme(legend.position="bottom", legend.box = "horizontal") +
  labs(title = "E-GARCH",
       subtitle = "Parameter estimatives",
       caption = "Parameters are expressed as variation with respect to 2013-01-02",
       x = NULL,
       y = NULL)

ggsave("EGARCH_parameters.png", plot = g1, device = "png", path = "./Trabalho/Plots/",
       scale = 2, width = 6, height = 3, units = "in",
       dpi = 72)
                       


# Avaliacao das previsao versos Realizado
preds <- as.data.frame(mdl)
preds$Date = zoo::as.Date(rownames(preds))
preds$e = preds$Realized - preds$Mu
preds$e2 = preds$e^2

preds = as_tibble(preds)
head(preds)

g1 = preds %>% ggplot() +
  geom_line(aes(x=Date, y=Realized, colour = "Realized")) +
  geom_line(aes(x=Date, y=Mu+Sigma, colour = "EGARCH")) +
  geom_line(aes(x=Date, y=Mu-Sigma, colour = "EGARCH")) +
  scale_colour_manual(NULL,breaks = c("Realized", "EGARCH"),
                      values = c("blue", "grey45")) +
  theme(legend.position="bottom", legend.box = "horizontal") +
  labs(title = "E-GARCH",
       subtitle = "Forecast estimative",
       caption = NULL,
       x = NULL,
       y = NULL)

ggsave("EGARCH_Estimative.png", plot = g1, device = "png", path = "./Trabalho/Plots/",
       scale = 2, width = 6, height = 3, units = "in",
       dpi = 72)
