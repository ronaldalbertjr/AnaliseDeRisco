# -----------------------------------------------------------------------------------#
# Trabalho Final - ICP103 Análise de Risco 2021/2
# Algoritmo 1: Simulação de Monte Carlo para valores futuros de opções modeladas por 
#               Movimento Browniano Geométrico
# Autores:
#   - Arthur Durso
#   - Luiz Palhano
#   - Ronald Albert de Araújo
#   - Tomaz Cuber Guimarães
#
# Objetivo: 
# -----------------------------------------------------------------------------------#

library(quantmod)   # Importando a biblioteca de modelagem financeira quantitativa 'quantmod'
getSymbols("WEGE3.SA", src = "yahoo", from = as.Date("2007-05-01"))     # Salva no ambiente os dados históricos de preço da ação WEGE3 do período de 01/05/2007 até a data atual
price_history <- as.numeric(WEGE3.SA$WEGE3.SA.Close)    # Conversão dos valores obtidos no passo anterior de fatores para numéricos, armazenando no data frame (ou array?) 'price_history'
log_return_price_history <- diff(log(price_history))[2:length(price_history) - 1]  

u <- mean(log_return_price_history)
var <- var(log_return_price_history)
drift <- u - (0.5 * var)
std <- sqrt(var)

t_intervals <- 1000
iterations <- 10

random <- matrix(qnorm(runif(iterations*t_intervals)), nrow=t_intervals)
daily_returns <- exp(drift + std * random)

S0 = price_history[length(price_history)]
future_prices <- matrix(replicate(iterations*t_intervals, S0), nrow=t_intervals)

for (i in 2:t_intervals) {
  future_prices[i,] = future_prices[i - 1,]*daily_returns[i,]
}

df <- data.frame(future_prices)

plot(1:t_intervals, df[,1], type = 'l', col=1)
for (i in 2:iterations) {
  lines(1:t_intervals, df[,i], type = 'l', col=i)
}
