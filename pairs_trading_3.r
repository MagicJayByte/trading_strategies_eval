library(quantmod)
library(dplyr)
setwd("C:/Users/Maciek/SGH_Magisterka")  # Set your working directory

library(yfR)

download_sp500_weekly <- function(tickers, start = "2010-01-01", end = "2024-12-31") {
  yf_get(tickers = tickers, first_date = start, last_date = end, freq_data = "weekly")
}

# Example usage:
weekly_data <- download_sp500_weekly(top_20, start = "2010-01-01", end = "2024-12-31")
top_20 <- c("AAPL", "MSFT", "AMZN", "NVDA", "GOOGL", "AMD", "TSLA", "META", 
            "BRK-B", "UNH", "JNJ", "JPM", "V", "PG", "MA", "HD", "CVX", 
            "LLY", "PFE", "XOM")

head(weekly_data)
weekly_list <- split(weekly_data, weekly_data$ticker)

for (symbol in top_20) {
  if (!(symbol %in% names(weekly_list))) {
    stop(paste("Ticker", symbol, "not found in weekly data."))
  }
  else {
    # Rename column price_adjusted to symbol.Adjusted
    colnames(weekly_list[[symbol]])[colnames(weekly_list[[symbol]]) == "price_adjusted"] <- paste0(symbol, ".Adjusted")
    # Convert date column to Date type and rename it to Date
    weekly_list[[symbol]]$Date <- as.Date(weekly_list[[symbol]]$date)
  }
}

save(sp500_data_monthly, file = "sp500_data_monthly.RData")
load("sp500_data_monthly.RData")

start_date_monthly_hist <- as.Date("2010-01-01")
end_date_monthly_hist <- as.Date("2010-12-31")

adjusted_list <- list()

for (symbol in top_20) {
  adj_col <- paste0(symbol, ".Adjusted")
  if (!(adj_col %in% colnames(sp500_data_monthly[[symbol]]))) {
    stop(paste("Adjusted column for", symbol, "not found in data."))} 
  else {
    date_col <- rownames(sp500_data_monthly[[symbol]])
    adjusted_prices <- sp500_data_monthly[[symbol]][[adj_col]]
    adjusted_list[[symbol]] <- data.frame(
      Date = as.Date(date_col), 
      Adjusted = adjusted_prices)
    colnames(adjusted_list[[symbol]])[2] <- paste0(symbol, ".Adjusted")
  }
}

sp500_data_adj <- Reduce(function(x, y) full_join(x, y, by = "Date"), adjusted_list)
sp500_data_adj <- sp500_data_adj[order(sp500_data_adj$Date), ]

save(sp500_data_adj, file = "sp500_data_adj.RData")
load("sp500_data_adj.RData")

sp500_data_adj_hist <- sp500_data_adj %>%
  filter(Date >= start_date_hist & Date <= end_date_hist)

for ( symbol in top_20) {
  adj_col <- paste0(symbol, ".Adjusted")
  if (!(adj_col %in% colnames(sp500_data_adj_hist))) {
    stop(paste("Adjusted column for", symbol, "not found in historical data."))}
  else {
    cumret_col <- sp500_data_adj_hist[[adj_col]] / sp500_data_adj_hist[[adj_col]][1]
    sp500_data_adj_hist[[paste0(symbol, "_cumret")]] <- cumret_col}}

colnames(sp500_data_adj_hist)


sp500_data_adj_hist$AMD_NVDA   <- sp500_data_adj_hist$AMD_cumret - sp500_data_adj_hist$NVDA_cumret
sp500_data_adj_hist$V_MA       <- sp500_data_adj_hist$V_cumret - sp500_data_adj_hist$MA_cumret
sp500_data_adj_hist$TSLA_NVDA  <- sp500_data_adj_hist$TSLA_cumret - sp500_data_adj_hist$NVDA_cumret
sp500_data_adj_hist$GOOGL_META <- sp500_data_adj_hist$GOOGL_cumret - sp500_data_adj_hist$META_cumret
sp500_data_adj_hist$BRKB_JPM   <- sp500_data_adj_hist$`BRK-B_cumret` - sp500_data_adj_hist$JPM_cumret
sp500_data_adj_hist$AMZN_AAPL  <- sp500_data_adj_hist$AMZN_cumret - sp500_data_adj_hist$AAPL_cumret
sp500_data_adj_hist$LLY_XOM    <- sp500_data_adj_hist$LLY_cumret - sp500_data_adj_hist$XOM_cumret
sp500_data_adj_hist$CVX_HD     <- sp500_data_adj_hist$CVX_cumret - sp500_data_adj_hist$HD_cumret
sp500_data_adj_hist$UNH_JNJ    <- sp500_data_adj_hist$UNH_cumret - sp500_data_adj_hist$JNJ_cumret
sp500_data_adj_hist$PG_PFE     <- sp500_data_adj_hist$PG_cumret - sp500_data_adj_hist$PFE_cumret

head(sp500_data_adj_hist)

pair_names <- c("AMD_NVDA", "V_MA", "TSLA_NVDA", "GOOGL_META", 
                "BRKB_JPM", "AMZN_AAPL", "LLY_XOM", "CVX_HD", "UNH_JNJ", "PG_PFE")

pair_stats <- data.frame(Pair = pair_names, Mean = NA_real_, SD = NA_real_, ZScore = NA_real_)

for (i in seq_along(pair_names)) {
  pair <- pair_names[i]
  values <- sp500_data_adj_hist[[pair]]
  pair_stats$Mean[i] <- mean(values, na.rm = TRUE)
  pair_stats$SD[i] <- sd(values, na.rm = TRUE)}

start_date_strat <- as.Date("2023-01-01")
end_date_strat <- as.Date("2024-12-31")

sp500_data_adj_strat <- sp500_data_adj %>%
  filter(Date >= start_date_strat & Date <= end_date_strat)

head(sp500_data_adj_strat)

for ( symbol in top_20) {
  adj_col <- paste0(symbol, ".Adjusted")
  if (!(adj_col %in% colnames(sp500_data_adj_strat))) {
    stop(paste("Adjusted column for", symbol, "not found in historical data."))}
  else {
    cumret_col <- sp500_data_adj_strat[[adj_col]] / sp500_data_adj_strat[[adj_col]][1]
    sp500_data_adj_strat[[paste0(symbol, "_cumret")]] <- cumret_col}}

sp500_data_adj_strat$AMD_NVDA   <- sp500_data_adj_strat$AMD_cumret - sp500_data_adj_strat$NVDA_cumret
sp500_data_adj_strat$V_MA       <- sp500_data_adj_strat$V_cumret - sp500_data_adj_strat$MA_cumret
sp500_data_adj_strat$TSLA_NVDA  <- sp500_data_adj_strat$TSLA_cumret - sp500_data_adj_strat$NVDA_cumret
sp500_data_adj_strat$GOOGL_META <- sp500_data_adj_strat$GOOGL_cumret - sp500_data_adj_strat$META_cumret
sp500_data_adj_strat$BRKB_JPM   <- sp500_data_adj_strat$`BRK-B_cumret` - sp500_data_adj_strat$JPM_cumret
sp500_data_adj_strat$AMZN_AAPL  <- sp500_data_adj_strat$AMZN_cumret - sp500_data_adj_strat$AAPL_cumret
sp500_data_adj_strat$LLY_XOM    <- sp500_data_adj_strat$LLY_cumret - sp500_data_adj_strat$XOM_cumret
sp500_data_adj_strat$CVX_HD     <- sp500_data_adj_strat$CVX_cumret - sp500_data_adj_strat$HD_cumret
sp500_data_adj_strat$UNH_JNJ    <- sp500_data_adj_strat$UNH_cumret - sp500_data_adj_strat$JNJ_cumret
sp500_data_adj_strat$PG_PFE     <- sp500_data_adj_strat$PG_cumret - sp500_data_adj_strat$PFE_cumret


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
  sp500_data_adj_strat[[signal_col]][sp500_data_adj_strat[[z_score_col]] < -2] <- 1
  # Sell signal when Z-Score is above 2
  sp500_data_adj_strat[[signal_col]][sp500_data_adj_strat[[z_score_col]] > 2] <- -1}


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
    scale_color_manual(values = c("1" = "green", "-1" = "red"),
                       labels = c("1" = "Buy Signal", "-1" = "Sell Signal")) +
    theme(legend.title = element_blank())
  
    ggsave(
    filename = paste0("pairs_trading_zscore_signals_", pair, ".png"),
    plot = p,
    width = 10,
    height = 6
  )}

