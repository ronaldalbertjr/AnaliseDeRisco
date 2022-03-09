# -----------------------------------------------------------------------------------#
# Trabalho Final - ICP103 Análise de Risco 2021/2
# Algoritmo 2: Modelagem Binomial de preço futuro de opções  
# Autores:
#   - Arthur Durso
#   - Luiz Palhano
#   - Ronald Albert de Araújo
#   - Tomaz Cuber Guimarães
#
# Objetivo: 
# -----------------------------------------------------------------------------------#
library(quantmod)                                                                       # Importando a biblioteca de modelagem financeira quantitativa 'quantmod'
getSymbols("GOOGL", src = "yahoo", from = as.Date("2020-05-01"))                        # Salva no ambiente os dados históricos de preço da ação GOOGL
                                                                                        # do período de 01/05/2020 até a data atual
price_history <- as.numeric(GOOGL$GOOGL.Close)                                          # Conversão dos valores obtidos no passo anterior de fatores para numéricos,
                                                                                        # armazenando no data frame (ou array?) 'price_history'
log_return_price_history <- diff(log(price_history))[2:length(price_history) - 1]
sigma <- sd(log_return_price_history)


build_stock_tree <- function(S, sigma, delta_t, N) {
  tree = matrix(0, nrow=N+1, ncol=N+1)
  U = exp(sigma*sqrt(delta_t))
  D = exp(-sigma*sqrt(delta_t))
  for (i in 1:(N+1)) {
    for (j in 1:i) {
      tree[i, j] = S * U^(j-1) * D^((i-1)-(j-1))
    }  }
  return(tree)
}


firstExample <- build_stock_tree(S=price_history[1], sigma=sigma, delta_t=1, N=20)  
View(firstExample)
