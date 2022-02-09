library(quantmod)
getSymbols("GOOGL", src = "yahoo", from = as.Date("2020-05-01"))
price_history <- as.numeric(GOOGL$GOOGL.Close)
log_return_price_history <- diff(log(price_history))[2:length(price_history) - 1]
sigma <- sd(log_return_price_history)/sqrt(length(price_history))


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
