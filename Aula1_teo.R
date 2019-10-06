# Clear variables
rm(list = ls())

# Used libraries
library(readxl)
library(ggplot2)
library(dplyr)
library(cowplot)
library(xts)
library(PerformanceAnalytics)

# Load Database
database <- read_excel("database/dados3_2018.xlsx")

head(database)
# Primeiro  vamos  apresentar a FAC e FACP  dos  quadrados  dos  retornos
# FAC e FACP  dos  quadrados  dos  retornos  do IBOV


p1 <- ggplot(database, aes(x = Data, y = IBOV_SB)) + geom_line(colour="blue") 
p2 <- ggplot(database, aes(x = Data, y = IBRX_SB)) + geom_line(colour="red")
p3 <- ggplot(database, aes(x = Data, y = SP500_SB)) + geom_line(colour="royal blue")
p4 <- ggplot(database, aes(x = Data, y = DJI_SB)) + geom_line(colour="black")

plot_grid(p1, p2, p3, p4)

rm(list = c("p1", "p2", "p3", "p4"))

dados <- xts(database[,-1], order.by = Data)

dados <- database %>% select(IBOV_SB, IBRX_SB, SP500_SB, DJI_SB, Data) %>% arrange(Data)

# Retornos simples e compostos para os ?ndices de Bolsa
dados = dados %>% mutate(rs_ibov = (IBOV_SB - lag(IBOV_SB))/lag(IBOV_SB),
                 rs_ibrx = (IBRX_SB - lag(IBRX_SB))/lag(IBRX_SB),
                 rs_sp500 = (SP500_SB - lag(SP500_SB))/lag(SP500_SB),
                 rs_dji = (DJI_SB - lag(DJI_SB))/lag(DJI_SB),
                 rc_ibov = (log(IBOV_SB) - log(lag(IBOV_SB))),
                 rc_ibrx  = (log(IBRX_SB) - log(lag(IBRX_SB))),
                 rc_sp500 = (log(SP500_SB) - log(lag(SP500_SB))),
                 rc_dji   = (log(DJI_SB) - log(lag(DJI_SB)))
                 )

# Retornos simples e compostos percentuais dos ?ndices de bolsa
dados$rsp_ibov  <- 100 * dados$rs_ibov
dados$rsp_ibrx  <- 100 * dados$rs_ibrx
dados$rsp_sp500 <- 100 * dados$rs_sp500
dados$rsp_dji   <- 100 * dados$rs_dji
dados$rcp_ibov  <- 100 * dados$rc_ibov
dados$rcp_ibrx  <- 100 * dados$rc_ibrx
dados$rcp_sp500 <- 100 * dados$rc_sp500
dados$rcp_dji   <- 100 * dados$rc_dji

