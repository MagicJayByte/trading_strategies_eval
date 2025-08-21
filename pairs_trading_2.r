library(quantmod)
library(dplyr)
setwd("C:/Users/Maciek/SGH_magisterka")
# setwd("C:/Users/mlube/trading_strategies_eval") 

top_10 <- symbols <- c(
  "AAPL",   # Apple
  "MSFT",   # Microsoft
  "NVDA",   # NVIDIA
  "AMD",    # Advanced Micro Devices
  "GOOGL",  # Alphabet (Google)
  "META",   # Meta Platforms (Facebook)
  "ADBE",   # Adobe
  "CSCO",   # Cisco Systems
  "ORCL",   # Oracle
  "CRM"     # Salesforce
)

load("sp500_data.RData")
ls()
names(sp500_data)

start_date_hist <- as.Date("2022-01-01")
end_date_hist <- as.Date("2022-12-31")

adjusted_list <- list()

for (symbol in top_10) {
  adj_col <- paste0(symbol, ".Adjusted")
  if (!(adj_col %in% colnames(sp500_data[[symbol]]))) {
    stop(paste("Adjusted column for", symbol, "not found in data."))} 
  else {
    date_col <- rownames(sp500_data[[symbol]])
    adjusted_prices <- sp500_data[[symbol]][[adj_col]]
    adjusted_list[[symbol]] <- data.frame(
      Date = as.Date(date_col), 
      Adjusted = adjusted_prices)
    colnames(adjusted_list[[symbol]])[2] <- paste0(symbol, ".Adjusted")
  }}


sp500_data_adj <- Reduce(function(x, y) full_join(x, y, by = "Date"), adjusted_list)
sp500_data_adj <- sp500_data_adj[order(sp500_data_adj$Date), ]

save(sp500_data_adj, file = "sp500_data_adj.RData")
load("sp500_data_adj.RData")

sp500_data_adj_hist <- sp500_data_adj %>%
  filter(Date >= start_date_hist & Date <= end_date_hist)

for ( symbol in top_10) {
  adj_col <- paste0(symbol, ".Adjusted")
  if (!(adj_col %in% colnames(sp500_data_adj_hist))) {
    stop(paste("Adjusted column for", symbol, "not found in historical data."))}
  else {
    cumret_col <- sp500_data_adj_hist[[adj_col]] / sp500_data_adj_hist[[adj_col]][1]
    sp500_data_adj_hist[[paste0(symbol, "_cumret")]] <- cumret_col}}



# Plot cumulative returns for each stock
library(ggplot2)
cumulative_returns_list <- list()
for (symbol in top_10) {
  cumret_col <- paste0(symbol, "_cumret")
  cumulative_returns_list[[symbol]] <- data.frame(
    Date = sp500_data_adj_hist$Date,
    Cumulative.Returns = sp500_data_adj_hist[[cumret_col]]
  )
}
cumulative_returns_df <- do.call(rbind, lapply(names(cumulative_returns_list), function(symbol) {
  data <- cumulative_returns_list[[symbol]]
  data$ticker <- symbol
  return(data)
}))
p <- ggplot(cumulative_returns_df, aes(x = Date, y = Cumulative.Returns, color = ticker)) +
  geom_line() +
  labs(title = "Skumulowane Zwroty 10 Najlepszych Akcji S&P 500",
       x = "Data",
       y = "Skumulowane Zwroty",
       color = "Spółka") +
  theme_minimal()

ggsave("top_10_cumulative_returns.png", plot = p, width = 10, height = 6)

colnames(sp500_data_adj_hist)

# Generate all unique pairs (order matters: A_B != B_A)
pair_combos <- t(combn(symbols, 2))

# For each pair, create a new column with the difference of their _cumret columns
for (i in seq_len(nrow(pair_combos))) {
  sym1 <- pair_combos[i, 1]
  sym2 <- pair_combos[i, 2]
  colname <- paste0(sym1, "_", sym2)
  col1 <- paste0(sym1, "_cumret")
  col2 <- paste0(sym2, "_cumret")
  sp500_data_adj_hist[[colname]] <- sp500_data_adj_hist[[col1]] - sp500_data_adj_hist[[col2]]
}

head(sp500_data_adj_hist)
sp500_data_adj_hist_colnames <- colnames(sp500_data_adj_hist)
sp500_data_adj_hist_colnames
pair_names <- sp500_data_adj_hist_colnames[!grepl("\\.Adjusted|_cumret|Date$", sp500_data_adj_hist_colnames)]

pair_stats <- data.frame(Pair = pair_names, Mean = NA_real_, SD = NA_real_)

for (i in seq_along(pair_names)) {
  pair <- pair_names[i]
  values <- sp500_data_adj_hist[[pair]]
  pair_stats$Mean[i] <- mean(values, na.rm = TRUE)
  pair_stats$SD[i] <- sd(values, na.rm = TRUE)
}
start_date_strat <- as.Date("2023-01-01")
end_date_strat <- as.Date("2023-06-30")

sp500_data_adj_strat <- sp500_data_adj %>%
  filter(Date >= start_date_strat & Date <= end_date_strat)

head(sp500_data_adj_strat)

for ( symbol in top_10) {
  adj_col <- paste0(symbol, ".Adjusted")
  if (!(adj_col %in% colnames(sp500_data_adj_strat))) {
    stop(paste("Adjusted column for", symbol, "not found in historical data."))}
  else {
    cumret_col <- sp500_data_adj_strat[[adj_col]] / sp500_data_adj_strat[[adj_col]][1]
    sp500_data_adj_strat[[paste0(symbol, "_cumret")]] <- cumret_col}}

# Plot cumulative returns for each stock in the strategy period
cumulative_returns_list_strat <- list()
for (symbol in top_10) {
  cumret_col <- paste0(symbol, "_cumret")
  cumulative_returns_list_strat[[symbol]] <- data.frame(
    Date = sp500_data_adj_strat$Date,
    Cumulative.Returns = sp500_data_adj_strat[[cumret_col]]
  )
}
cumulative_returns_df_strat <- do.call(rbind, lapply(names(cumulative_returns_list_strat), function(symbol) {
  data <- cumulative_returns_list_strat[[symbol]]
  data$ticker <- symbol
  return(data)
}))
p_strat <- ggplot(cumulative_returns_df_strat, aes(x = Date, y = Cumulative.Returns, color = ticker)) +
  geom_line() +
  labs(title = "Skumulowane Zwroty 10 Najlepszych Akcji S&P 500 (Strategia)",
       x = "Data",
       y = "Skumulowane Zwroty",
       color = "Spółki") +
  theme_minimal()
ggsave("top_10_cumulative_returns_strat.png", plot = p_strat, width = 10, height = 6)

# Calculate differences for each pair in the strategy period
for (pair in pair_names) {
  sym1 <- strsplit(pair, "_")[[1]][1]
  sym2 <- strsplit(pair, "_")[[1]][2]
  col1 <- paste0(sym1, "_cumret")
  col2 <- paste0(sym2, "_cumret")
  sp500_data_adj_strat[[pair]] <- sp500_data_adj_strat[[col1]] - sp500_data_adj_strat[[col2]]
}

colnames(sp500_data_adj_strat)

# Calculate Z-Score for each day in sp500_data_adj_hist
for (pair in pair_names) {
  mean_val <- pair_stats$Mean[pair_stats$Pair == pair]
  sd_val <- pair_stats$SD[pair_stats$Pair == pair]
  z_score_col <- paste0(pair, "_ZScore")
  sp500_data_adj_strat[[z_score_col]] <- (sp500_data_adj_strat[[pair]] - mean_val) / sd_val}

# Create flags for pairs trading signals
for (pair in pair_names) {
  z_score_col <- paste0(pair, "_ZScore")
  signal_col <- paste0(pair, "_Signal")
  sp500_data_adj_strat[[signal_col]] <- NA_integer_
  # Buy signal when Z-Score is below -2
  sp500_data_adj_strat[[signal_col]][sp500_data_adj_strat[[z_score_col]] < -2] <- 'LONG_SHORT'
  # Sell signal when Z-Score is above 2
  sp500_data_adj_strat[[signal_col]][sp500_data_adj_strat[[z_score_col]] > 2] <- "SHORT_LONG"
#   # Exit signal when Z-Score is between -0.2 and 0.2
# sp500_data_adj_strat[[signal_col]][sp500_data_adj_strat[[z_score_col]] >= -0.2 & sp500_data_adj_strat[[z_score_col]] <= 0.2] <- "EXIT"
}

# For each pair plot difference, Z-Score, and signals
library(ggplot2)
for (pair in pair_names) {
  diff_col <- paste0(pair, "_ZScore")
  signal_col <- paste0(pair, "_Signal")
  
  # Convert signal column to factor for plotting
  sp500_data_adj_strat[[signal_col]] <- as.factor(sp500_data_adj_strat[[signal_col]])

  p <- ggplot(sp500_data_adj_strat, aes(x = Date, y = .data[[diff_col]])) +
    geom_line(color = "blue") +
    geom_hline(yintercept = c(-2, 2), linetype = "dashed", color = "red") +
    geom_point(aes(color = .data[[signal_col]]), size = 1.5) +
    labs(title = paste("Z-Score for", pair), x = "Date", y = "Z-Score") +
    theme_minimal() +
    scale_color_manual(values = c("LONG_SHORT" = "green", "SHORT_LONG" = "red"),
                       labels = c("LONG_SHORT" = "Long/Short Signal", "SHORT_LONG" = "Short/Long Signal")) +
    theme(legend.title = element_blank())
  
    ggsave(
    filename = paste0("pairs_trading_zscore_signals_", pair, ".png"),
    plot = p,
    width = 10,
    height = 6
  )
}

colnames(sp500_data_adj_strat)

# install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

sp500_data_adj_strat <- sp500_data_adj_strat[order(sp500_data_adj_strat$Date), ]
portfolio_list <- list()
for (pair in pair_names) {
  pairs_singal_col <- paste0(pair, "_Signal")
  first_cumret_col <- paste0(strsplit(pair, "_")[[1]][1], "_cumret")
  second_cumret_col <- paste0(strsplit(pair, "_")[[1]][2], "_cumret")
  spread_zscore <- paste0(pair, "_ZScore")
  trade_cost <- 0.0001
  portfolio <- list(
    pair_name = pair,
    enter_cumret_first_stock = NA_real_,
    enter_cumret_second_stock = NA_real_,
    current_position = NULL,
    returns = xts(order.by = as.Date(character())),
    Sharpe = NA_real_,
    current_equity = 1000,
    sum_returns = NA_real_,
    mean_returns = NA_real_,
    is_converged = FALSE,
    num_of_trades = 0
  )
  for (i in 1:nrow(sp500_data_adj_strat)) {
    current_signal <- sp500_data_adj_strat[[pairs_singal_col]][i]
    current_position <- portfolio$current_position
    if (is.null(current_position) || is.na(current_position) || current_position == "") {
      if (is.na(current_signal) || current_signal == "") {
        next
      } else if (current_signal == "LONG_SHORT" || current_signal == "SHORT_LONG") {
        portfolio$enter_cumret_first_stock <- sp500_data_adj_strat[[first_cumret_col]][i]
        portfolio$enter_cumret_second_stock <- sp500_data_adj_strat[[second_cumret_col]][i]
        portfolio$current_position <- current_signal
      }
    } else if (current_position == "SHORT_LONG") {
      if ((sp500_data_adj_strat[[spread_zscore]][i] < 0) || (i == nrow(sp500_data_adj_strat))) {
        long_pos_return <- log(sp500_data_adj_strat[[second_cumret_col]][i]) - log(portfolio$enter_cumret_second_stock)
        short_pos_return <- log(portfolio$enter_cumret_first_stock) - log(sp500_data_adj_strat[[first_cumret_col]][i])
        ret_value <- 0.5 * (long_pos_return + short_pos_return) - trade_cost  
        ret_date <- sp500_data_adj_strat$Date[i]
        portfolio$returns <- rbind(portfolio$returns, xts(ret_value, order.by = ret_date))
        portfolio$current_equity <- portfolio$current_equity * (1 + ret_value)
        portfolio$current_position <- NULL
      } else if (sp500_data_adj_strat[[spread_zscore]][i] > 0) {
        next
      } else {
        print("Missing Data on SHORT_LONG position")
      }
    } else if (current_position == "LONG_SHORT") {
      if ((sp500_data_adj_strat[[spread_zscore]][i] >= 0) || (i == nrow(sp500_data_adj_strat))) {
        long_pos_return <- log(sp500_data_adj_strat[[first_cumret_col]][i]) - log(portfolio$enter_cumret_first_stock)
        short_pos_return <- log(portfolio$enter_cumret_second_stock) - log(sp500_data_adj_strat[[second_cumret_col]][i])
        ret_value <- 0.5 * (long_pos_return + short_pos_return) - trade_cost
        ret_date <- sp500_data_adj_strat$Date[i]
        portfolio$returns <- rbind(portfolio$returns, xts(ret_value, order.by = ret_date))
        portfolio$current_equity <- portfolio$current_equity * (1 + (ret_value))
        portfolio$current_position <- NULL
      } else if (sp500_data_adj_strat[[spread_zscore]][i] < 0) {
        next
      } else {
        print("Missing Data on SHORT_LONG position")
      }
    }
  }

if (is.xts(portfolio$returns) && nrow(portfolio$returns) > 1) {
  portfolio$Sharpe <- SharpeRatio.annualized(portfolio$returns, Rf = 0)
} else {
  portfolio$Sharpe <- 0
}
print(portfolio$pair_name)
print(nrow(portfolio$returns))
print(portfolio$returns)
portfolio$is_converged <- nrow(portfolio$returns) > 1
portfolio$num_of_trades <- nrow(portfolio$returns)
portfolio$mean_returns <- mean(portfolio$returns, na.rm = TRUE)
portfolio$sum_returns <- sum(portfolio$returns, na.rm = TRUE)
portfolio_list[[pair]] <- portfolio
}

converged_portfolios <- sapply(portfolio_list, function(x) x$is_converged)
converged_count <- sum(converged_portfolios, na.rm = TRUE)
total_trades_converged <- sum(sapply(portfolio_list[converged_portfolios], function(x) x$num_of_trades), na.rm = TRUE)
average_trades_per_month <- total_trades_converged / 6
average_trades_converged <- mean(sapply(portfolio_list[converged_portfolios], function(x) x$num_of_trades), na.rm = TRUE)
converged_returns <- sapply(portfolio_list[converged_portfolios], function(x) x$sum_returns)
mean_converged_returns <- mean(converged_returns, na.rm = TRUE)
mean_converged_returns
median_converged_returns <- median(converged_returns, na.rm = TRUE)
median_converged_returns
kurtosis_converged_returns <- kurtosis(converged_returns, na.rm = TRUE)
kurtosis_converged_returns
skewness_converged_returns <- skewness(converged_returns, na.rm = TRUE)
skewness_converged_returns
converged_count

#Provide the same stats for unconverged portfolios
unconverged_portfolios <- !converged_portfolios
unconverged_count <- sum(unconverged_portfolios, na.rm = TRUE)
total_trades_unconverged <- sum(sapply(portfolio_list[unconverged_portfolios], function(x) x$num_of_trades), na.rm = TRUE)
average_trades_unconverged <- mean(sapply(portfolio_list[unconverged_portfolios], function(x) x$num_of_trades), na.rm = TRUE)
unconverged_returns <- sapply(portfolio_list[unconverged_portfolios], function(x) x$sum_returns)
mean_unconverged_returns <- mean(unconverged_returns, na.rm = TRUE)
median_unconverged_returns <- median(unconverged_returns, na.rm = TRUE)
kurtosis_unconverged_returns <- kurtosis(unconverged_returns, na.rm = TRUE)
skewness_unconverged_returns <- skewness(unconverged_returns, na.rm = TRUE)

mean_unconverged_returns
median_unconverged_returns
kurtosis_unconverged_returns
skewness_unconverged_returns

# MSFT GOOGL cumret plot
windows()
plot(sp500_data_adj_strat$Date, sp500_data_adj_strat$MSFT_cumret, type = "l", col = "blue", ylim = c(0, 1.5), 
     xlab = "Data", ylab = "Skumulowane Zwroty", main = "Skumulowane Zwroty MSFT i GOOGL")
lines(sp500_data_adj_strat$Date, sp500_data_adj_strat$GOOGL_cumret, col = "red")
legend("topleft", legend = c("MSFT", "GOOGL"), col = c("blue", "red"), lty = 1)


converged_returns

mean_unconverged_returns
median_unconverged_returns
kurtosis_unconverged_returns
skewness_unconverged_returns
unconverged_count

library(moments)
sharpe_ratios <- sapply(portfolio_list, function(x) x$Sharpe)
median_sharpe <- median(sharpe_ratios, na.rm = TRUE)
average_sharpe <- mean(sharpe_ratios, na.rm = TRUE)
skewness_sharpe <- skewness(sharpe_ratios, na.rm = TRUE)
kurtosis_sharpe <- kurtosis(sharpe_ratios, na.rm = TRUE)

# Sharpe ratios for converged portfolios
converged_sharpe_ratios <- sharpe_ratios[converged_portfolios]
mean_converged_sharpe <- mean(converged_sharpe_ratios, na.rm = TRUE)

mean_converged_sharpe

returns_list <- sapply(portfolio_list, function(x) x$sum_returns)
mean_returns <- mean(returns_list, na.rm = TRUE)
median_returns <- median(returns_list, na.rm = TRUE)
skewness_returns <- skewness(returns_list, na.rm = TRUE)
kurtosis_returns <- kurtosis(returns_list, na.rm = TRUE)

returns_list

mean_returns
median_returns
  skewness_returns
kurtosis_returns

#Count the number of trades
total_trades <- sum(sapply(portfolio_list, function(x) x$num_of_trades), na.rm = TRUE)
average_trades <- mean(sapply(portfolio_list, function(x) x$num_of_trades), na.rm = TRUE)

# Average trades per month
average_trades_per_month <- total_trades / 6

total_trades
average_trades
total_trades / 9

# Returns for the converged portfolios


library(xts)
library(PerformanceAnalytics)

# 1. Collect all returns series into a list
returns_xts_list <- lapply(portfolio_list, function(x) x$returns)

returns_xts_list

# 2. Merge all returns series by Date (outer join)
all_returns_merged <- do.call(merge, c(returns_xts_list, all = TRUE))

all_returns_merged

# 3. Replace NAs with 0 (no trade on that date for that portfolio)
all_returns_merged[is.na(all_returns_merged)] <- 0

all_returns_merged
# 4. Sum returns across portfolios for each date
portfolio_total_returns <- xts(rowSums(all_returns_merged), order.by = index(all_returns_merged))

portfolio_total_returns

# 5. Calculate Sharpe Ratio for the summed portfolio
portfolio_sharpe <- SharpeRatio.annualized(portfolio_total_returns, Rf = 0)

# Output the Sharpe ratio
portfolio_sharpe

mean_ret <- mean(portfolio_total_returns, na.rm = TRUE)
sd_ret <- sd(portfolio_total_returns, na.rm = TRUE)
sharpe_simple <- mean_ret / sd_ret
sharpe_annualized <- sharpe_simple * sqrt(252)  # Use 252 for daily, 52 for weekly, 12 for monthly
sharpe_annualized

# Only for converged portfolios
converged_returns_xts_list <- lapply(portfolio_list[converged_portfolios], function(x) x$returns)

# Merge all converged returns series by Date (outer join)
converged_all_returns_merged <- do.call(merge, c(converged_returns_xts_list, all = TRUE))

# Replace NAs with 0 (no trade on that date for that portfolio)
converged_all_returns_merged[is.na(converged_all_returns_merged)] <- 0

# Sum returns across converged portfolios for each date
converged_portfolio_total_returns <- xts(rowSums(converged_all_returns_merged), order.by = index(converged_all_returns_merged))

# Calculate Sharpe Ratio for the summed converged portfolio
converged_portfolio_sharpe <- SharpeRatio.annualized(converged_portfolio_total_returns, Rf = 0)

# Output the Sharpe ratio
converged_portfolio_sharpe

windows()
hist(
  sharpe_ratios,
  main = "Sharpe Ratios of Pairs Trading Strategies",
  xlab = "Wartości Sharpe'a",
  ylab = "Częstotliwość",
  col = "blue",
  breaks = 10
)
legend("topright", legend = paste("Mean =", round(average_sharpe, 2)), 
       col = "green", lwd = 2)

windows()
hist(
  returns_list,
  main = "Returns of Pairs Trading Strategies",
  xlab = "Zwroty",
  ylab = "Częstotliwość",
  col = "blue",
  breaks = 10
)



sharpe_ratios
returns_list











