# Apple stock analysis

# Load the indicator functions
source("sma.R")
source("ema.R")
source("macd.R")
source("rsi.R")
source("crossover.R")

# Load and sort the data by date
apple <- read.csv("AAPL_data.csv")
apple$Date <- as.Date(apple$Date)
apple <- apple[order(apple$Date), ]

# Calculate indicators using closing prices
apple$EMA_10 <- ema(apple$Close, 10)
apple$EMA_20 <- ema(apple$Close, 20)
apple$RSI_14 <- rsi(apple$Close, 14)

macd_result <- macd(apple$Close, 12, 26, 9)
apple$MACD <- macd_result$macd_line
apple$MACD_Signal <- macd_result$signal_line

# SMA returns fewer values, so add NA for the first 9 dates
apple$SMA_10 <- c(rep(NA, 9), sma(apple$Close, 10))

# Example trading signals from EMA crossovers
apple$Signal <- crossover(apple$EMA_10, apple$EMA_20)

# Show the latest results
tail(apple)

# Filter to December 2025
december <- subset(
  apple,
  Date >= as.Date("2025-12-01") &
    Date <= as.Date("2025-12-31")
)

# Convert crossover directions into example trading signals
apple$Trade <- ifelse(
  apple$Signal == "Up", "Buy",
  ifelse(apple$Signal == "Down", "Sell", "Hold")
)

# Display December data and rows with crossover signals
print(december[, c("Date", "Close", "SMA_10", "RSI_14")])

trades <- subset(apple, Trade != "Hold")
print(trades[, c("Date", "Close", "EMA_10", "EMA_20", "Trade")])

# Save the analysis results
write.csv(apple, "AAPL_analysis_results.csv", row.names = FALSE)