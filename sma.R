sma <- function (data, period) 
{
    if (period < 1 || period != as.integer(period)) {
        stop("Period must be a positive integer")
    }
    if (length(data) < period) {
        stop("Data length should be greater than or equal to the period")
    }
    sma_values <- numeric(length(data) - period + 1)
    for (i in seq_along(sma_values)) {
        sma_values[i] <- sum(data[i:(i + period - 1)])/period
    }
    return(sma_values)
}
