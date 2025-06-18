library(quantmod)
library(dplyr)
setwd("C:/Users/Maciek/SGH_Magisterka")  # Set your working directory

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
  sp500_data_adj_strat[[signal_col]][sp500_data_adj_strat[[z_score_col]] < -2] <- -1
  # Sell signal when Z-Score is above 2
  sp500_data_adj_strat[[signal_col]][sp500_data_adj_strat[[z_score_col]] > 2] <- "SELL"
  # Exit signal when Z-Score is between -1 and 1
  sp500_data_adj_strat[[signal_col]][sp500_data_adj_strat[[z_score_col]] >= -1 & sp500_data_adj_strat[[z_score_col]] <= 1] <- "EXIT"
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
    scale_color_manual(values = c("BUY" = "green", "SELL" = "red", "EXIT" = "purple"),
                       labels = c("BUY" = "Buy Signal", "SELL" = "Sell Signal", "EXIT" = "Exit Signal")) +
    theme(legend.title = element_blank())
  
    ggsave(
    filename = paste0("pairs_trading_zscore_signals_", pair, ".png"),
    plot = p,
    width = 10,
    height = 6
  )
}

colnames(sp500_data_adj_strat)

# Set positions based on signals for each pair
for (pair in pair_names) {



