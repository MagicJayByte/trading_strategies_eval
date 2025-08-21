library(quantmod)
getwd()
setwd("C:/Users/Maciek/SGH_magisterka") # Change to your working directory
download_sp500_data <- function(start = "1990-01-01", end = "2020-12-31") {
  env <- new.env()
  getSymbols(c('KO', 'PEP'), src = "yahoo", from = start, to = end, env = env, auto.assign = TRUE)
  data_list <- eapply(env, function(x) as.data.frame(x))
  return(data_list)
}

# download_sp500_data <- function(start = "1970-01-01", end = "1990-12-31") {
#   env <- new.env()
#   getSymbols(c('KO', 'PEP'), src = "yahoo", from = start, to = end, env = env, auto.assign = TRUE)
#   data_list <- eapply(env, function(x) as.data.frame(x))
#   return(data_list)
# }


coc_pep = download_sp500_data()
coc_pep
class(coc_pep)


coc_pep_adj <- list()
for (stock in names(coc_pep)) {
  adj_col <- paste0(stock, ".Adjusted")
  adjusted_prices <- coc_pep[[stock]][[adj_col]]
  # Calculate simple daily returns
  simple_ret <- diff(adjusted_prices) / head(adjusted_prices, -1)
  # Calculate cumulative return index (start at 1)
  cumret <- cumprod(1 + simple_ret)
  coc_pep_adj[[stock]] <- data.frame(
    Date = as.Date(rownames(coc_pep[[stock]])[-1]), # Remove first date due to diff
    Cumulative.Returns = cumret
  )
}

names(coc_pep_adj$PEP)
head(coc_pep_adj$KO)

#plot cumreturns
library(ggplot2)
p <- ggplot() +
  geom_line(data = coc_pep_adj$KO, aes(x = Date, y = Cumulative.Returns, color = "KO")) +
  geom_line(data = coc_pep_adj$PEP, aes(x = Date, y = Cumulative.Returns, color = "PEP")) +
  labs(title = "Skumulowane Zwroty Pepsi i Coca-Coli",
       x = "Data",
       y = "Skumulowane Zwroty",
       color = 'Legenda') +
  scale_color_manual(values = c("KO" = "blue", "PEP" = "red")) +
  theme_minimal()
ggsave("KO_PEP_cumreturns.png", plot = p, width = 8, height = 5)


