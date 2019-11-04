ggplot_Acf_Pacf <- function(x){
  require(tibble)
  
  conf.level <- 0.95
  n = length(x)
  ciline <- qnorm((1 - conf.level)/2)/sqrt(n)
  
  acf_1 = acf(x, plot = F)
  pacf_1 = pacf(x, plot = F)
  
  acf_1_df =  tibble(lag = as.vector(acf_1$lag), ACF = as.vector(acf_1$acf)) 
  pacf_1_df =  tibble(lag = as.vector(pacf_1$lag), PACF = as.vector(pacf_1$acf)) 
  
  p1 <- ggplot(data=acf_1_df, mapping=aes(x=lag, y=ACF)) +
    geom_bar(stat = "identity", position = "identity", fill="lightsteelblue") + 
    geom_hline(yintercept = ciline, colour="red", size = 0.8, linetype="dashed") + 
    geom_hline(yintercept = -ciline, colour="red", size = 0.8, linetype="dashed") 
  
  p2 <- ggplot(data=pacf_1_df, mapping=aes(x=lag, y=PACF)) +
    geom_bar(stat = "identity", position = "identity", fill="lightsteelblue") + 
    geom_hline(yintercept = ciline, colour="red", size = 0.8, linetype="dashed") + 
    geom_hline(yintercept = -ciline, colour="red", size = 0.8, linetype="dashed")
  
  ret = list(ACF = p1, PACF = p2)
  
  return(ret)
}
