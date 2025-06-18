if (!require("quantmod")) install.packages("quantmod")
library(quantmod)
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
 
 top_10

download_sp500_data <- function(start = "2010-01-01", end = "2024-12-31") {
  env <- new.env()
  getSymbols(top_10, src = "yahoo", from = start, to = end, env = env, auto.assign = TRUE)
  data_list <- eapply(env, function(x) as.data.frame(x))
  return(data_list)
}

sp500_data <- download_sp500_data()

head(sp500_data$AAPL)
save(sp500_data, file = "sp500_data.RData")
load("sp500_data.RData")


head(sp500_data$AAPL)

# Create a data frame of adjusted returns for all symbols
returns_list <- list()

for (symbol in top_20) {
  adj_col <- paste0(symbol, ".Adjusted")
  if (adj_col %in% colnames(sp500_data[[symbol]])) {
    adj_prices <- sp500_data[[symbol]][[adj_col]]
    dates <- rownames(sp500_data[[symbol]])
    returns <- diff(log(adj_prices))
    # Remove the first date since diff reduces length by 1
    returns_list[[symbol]] <- data.frame(Date = dates[-1], Return = returns)
    colnames(returns_list[[symbol]])[2] <- symbol
  }
}

head(returns_list)

# Merge all into a single data frame by Date
library(dplyr)
returns_df <- Reduce(function(x, y) full_join(x, y, by = "Date"), returns_list)
returns_df <- returns_df[order(returns_df$Date), ]  # Sort by date

head(returns_df)

df_returns_22_24 <- returns_df %>%
  filter(Date >= "2022-01-01" & Date <= "2024-12-31")

head(df_returns_22_24)

colnames(df_returns_22_24)

diff_pairs <- data.frame(
  Date = df_returns_22_24$Date,
  AMD_NVDA   = df_returns_22_24$AMD - df_returns_22_24$NVDA,
  V_MA       = df_returns_22_24$V - df_returns_22_24$MA,
  TSLA_NVDA  = df_returns_22_24$TSLA - df_returns_22_24$NVDA,
  GOOGL_META = df_returns_22_24$GOOGL - df_returns_22_24$META,
  BRKB_JPM   = df_returns_22_24$`BRK-B` - df_returns_22_24$JPM,
  AMZN_AAPL  = df_returns_22_24$AMZN - df_returns_22_24$AAPL,
  LLY_XOM    = df_returns_22_24$LLY - df_returns_22_24$XOM,
  CVX_HD     = df_returns_22_24$CVX - df_returns_22_24$HD,
  UNH_JNJ    = df_returns_22_24$UNH - df_returns_22_24$JNJ
)

head(diff_pairs)
diff_pairs$Date <- as.Date(diff_pairs$Date)
# plot(diff_pairs$Date, diff_pairs$AMD_NVDA, type = "l", col = "blue", 
#      xlab = "Date", ylab = "Difference in Returns (AMD - NVDA)", 
#      main = "Difference in Returns: AMD vs NVDA")

for (col in names(diff_pairs)[-1]) { # skip Date
  diff_pairs[[paste0(col, "_mavg5")]]  <- rollapply(diff_pairs[[col]], 5, mean, fill = NA, align = "right")
  diff_pairs[[paste0(col, "_mavg20")]] <- rollapply(diff_pairs[[col]], 20, mean, fill = NA, align = "right")
  diff_pairs[[paste0(col, "_std5")]]   <- rollapply(diff_pairs[[col]], 5, sd, fill = NA, align = "right")
  diff_pairs[[paste0(col, "_std20")]]  <- rollapply(diff_pairs[[col]], 20, sd, fill = NA, align = "right")
}
diff_pairs <- diff_pairs[-(1:20), ]
head(diff_pairs)

for (col in names(diff_pairs)[2:10]) { # skip Date, select only original pairs
  diff_pairs[[paste0(col, "_zscore")]] <- 
    (diff_pairs[[paste0(col, "_mavg5")]] - diff_pairs[[paste0(col, "_mavg20")]]) / diff_pairs[[paste0(col, "_std20")]]
}
head(diff_pairs)

entry_threshold <- .8
exit_threshold <- 0.5

for (col in names(diff_pairs)[2:10]) { # for each pair
  z_col <- paste0(col, "_zscore")
  # Long entry: z-score < -entry_threshold
  diff_pairs[[paste0(col, "_long_entry")]] <- diff_pairs[[z_col]] < -entry_threshold
  # Short entry: z-score > entry_threshold
  diff_pairs[[paste0(col, "_short_entry")]] <- diff_pairs[[z_col]] > entry_threshold
  # Exit: abs(z-score) < exit_threshold
  diff_pairs[[paste0(col, "_exit")]] <- abs(diff_pairs[[z_col]]) < exit_threshold
}

head(diff_pairs)

library(ggplot2)

for (col in names(diff_pairs)[2:10]) { # for each pair
  z_col <- paste0(col, "_zscore")
  long_col <- paste0(col, "_long_entry")
  short_col <- paste0(col, "_short_entry")
  exit_col <- paste0(col, "_exit")
  
  p <- ggplot(diff_pairs, aes(x = Date, y = .data[[z_col]])) +
    geom_line(color = "blue") +
    geom_point(data = diff_pairs[diff_pairs[[long_col]], ], aes(x = Date, y = .data[[z_col]]), color = "green", size = 2) +
    geom_point(data = diff_pairs[diff_pairs[[short_col]], ], aes(x = Date, y = .data[[z_col]]), color = "red", size = 2) +
    geom_point(data = diff_pairs[diff_pairs[[exit_col]], ], aes(x = Date, y = .data[[z_col]]), color = "black", size = 2) +
    labs(title = paste("Z-Score for", col), x = "Date", y = "Z-Score") +
    theme_minimal() +
    theme(legend.position = "none")
  
  ggsave(paste0("zscore_plot_", col, ".png"), plot = p, width = 10, height = 6)
}


names(diff_pairs)
