library(quantmod)
library(dplyr)
setwd("C:/Users/Maciek/SGH_magisterka")  # Set your working directory

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
pair_stats

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
  sp500_data_adj_strat[[signal_col]][sp500_data_adj_strat[[z_score_col]] < -2] <- 'BUY'
  # Sell signal when Z-Score is above 20
  sp500_data_adj_strat[[signal_col]][sp500_data_adj_strat[[z_score_col]] > 2] <- "SELL"
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
    scale_color_manual(values = c("BUY" = "green", "SELL" = "red", "EXIT" = "black"),
                       labels = c("BUY" = "Buy Signal", "SELL" = "Sell Signal",  "EXIT" = 'black')) +
    theme(legend.title = element_blank())
  
    ggsave(
    filename = paste0("pairs_trading_zscore_signals_", pair, ".png"),
    plot = p,
    width = 10,
    height = 6
  )
}

colnames(sp500_data_adj_strat)

install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

sp500_data_adj_strat <- sp500_data_adj_strat[order(sp500_data_adj_strat$Date), ]
portfolio_list <- list()
for (pair in pair_names) {
  pairs_singal_col <- paste0(pair, "_Signal")
  first_cumret_col <- paste0(strsplit(pair, "_")[[1]][1], "_cumret")
  second_cumret_col <- paste0(strsplit(pair, "_")[[1]][2], "_cumret")
  spread_zscore <- paste0(pair, "_ZScore")
  portfolio <- list(
    pair_name = pair,
    enter_cumret_first_stock = NA_real_,
    enter_cumret_second_stock = NA_real_,
    current_position = NULL,
    returns = xts(order.by = as.Date(character())),
    Sharpe = NA_real_,
    final_return = NA_real_,
    current_equity = 1000
  )
  for (i in 1:nrow(sp500_data_adj_strat)) {
    current_signal <- sp500_data_adj_strat[[pairs_singal_col]][i]
    current_position <- portfolio$current_position
    if (is.null(current_position) || is.na(current_position) || current_position == "") {
      if (is.na(current_signal) || current_signal == "") {
        next
      } else if (current_signal == "BUY" || current_signal == "SELL") {
        portfolio$enter_cumret_first_stock <- sp500_data_adj_strat[[first_cumret_col]][i]
        portfolio$enter_cumret_second_stock <- sp500_data_adj_strat[[second_cumret_col]][i]
        portfolio$current_position <- current_signal
      }
    } else if (current_position == "BUY") {
      if ((sp500_data_adj_strat[[spread_zscore]][i] <= 0) || (i == nrow(sp500_data_adj_strat))) { 
        long_pos_return <- log(portfolio$enter_cumret_first_stock) - log(sp500_data_adj_strat[[first_cumret_col]][i])
        short_pos_return <- log(sp500_data_adj_strat[[second_cumret_col]][i]) - log(portfolio$enter_cumret_second_stock)
        ret_value <- 0.5 * (long_pos_return + short_pos_return)
        ret_date <- sp500_data_adj_strat$Date[i]
        portfolio$returns <- rbind(portfolio$returns, xts(ret_value, order.by = ret_date))
        portfolio$current_equity <- portfolio$current_equity * (1 + 0.5 * (long_pos_return + short_pos_return))
        portfolio$current_position <- NULL
      } else if (sp500_data_adj_strat[[spread_zscore]][i] > 0) {
        next  # Skip to the next iteration if already in a BUY position
      } else {
        print("Missing Data on BUY position")
      }
    } else if (current_position == "SELL") {
      if ((sp500_data_adj_strat[[spread_zscore]][i] >= 0) || (i == nrow(sp500_data_adj_strat))) {
        long_pos_return <- log(sp500_data_adj_strat[[second_cumret_col]][i]) - log(portfolio$enter_cumret_second_stock)
        short_pos_return <- log(portfolio$enter_cumret_first_stock) - log(sp500_data_adj_strat[[first_cumret_col]][i])
        ret_value <- 0.5 * (long_pos_return + short_pos_return)
        ret_date <- sp500_data_adj_strat$Date[i]
        portfolio$returns <- rbind(portfolio$returns, xts(ret_value, order.by = ret_date))
        portfolio$current_equity <- portfolio$current_equity * (1 + 0.5 * (long_pos_return + short_pos_return))
        portfolio$current_position <- NULL
      } else if (sp500_data_adj_strat[[spread_zscore]][i] < 0) {
        next  # Skip to the next iteration if already in a SELL position
      } else {
        print("Missing Data on SELL position")
      }
    }
  }
  portfolio$Sharpe <- SharpeRatio(portfolio$returns, Rf = 0.01, FUN = "StdDev")
  portfolio$final_return <- log(portfolio$current_equity) - log(1000)
  portfolio_list[[pair]] <- portfolio
}

names(portfolio_list)
head(portfolio_list)

names(sp500_data_adj_strat)

sharpe_ratios <- sapply(portfolio_list, function(x) x$Sharpe)
returns_list <- lapply(portfolio_list, function(x) x$returns)
final_returns <- sapply(portfolio_list, function(x) x$final_return)
sum_returns <- sapply(portfolio_list, function(x) sum(x$returns, na.rm = TRUE))
names(returns_list)

final_returns
sum_returns

windows()
hist(sharpe_ratios, main = "Sharpe Ratios of Pairs Trading Strategies", xlab = "Sharpe Ratio", col = "blue", breaks = 10)
median_sharpe <- median(sharpe_ratios, na.rm = TRUE)
average_sharpe <- mean(sharpe_ratios, na.rm = TRUE)
abline(v = average_sharpe, col = "green", lwd = 2)
legend("topright", legend = paste("Mean =", round(average_sharpe, 2)), 
       col = "green", lwd = 2)

windows()
hist(final_returns, main = "Returns of Pairs Trading Strategies", xlab = "Returns", col = "blue", breaks = 10)
median_returns <- median(final_returns, na.rm = TRUE)
average_returns <- mean(final_returns, na.rm = TRUE)
abline(v = average_returns, col = "green", lwd = 2)
legend("topright", legend = paste("Mean =", round(average_returns, 2)), 
       col = "green", lwd = 2)



sharpe_ratios
returns_list











