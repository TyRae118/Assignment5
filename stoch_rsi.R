source("sma.R")
source("rsi.R")
stoch_rsi <- function (data, period, k_period, d_period) 
{
    rsi_values <- rsi(data, period)
    rsi_values <- rsi_values[!is.na(rsi_values)]
    min_rsi <- min(rsi_values)
    max_rsi <- max(rsi_values)
    if (max_rsi == min_rsi) {
        stop("Stochastic RSI is undefined when all RSI values are equal")
    }
    k_values <- (rsi_values - min_rsi)/(max_rsi - min_rsi)
    k_line <- sma(k_values, k_period)
    d_line <- sma(k_line, d_period)
    return(list(k_line = k_line, d_line = d_line))
}
