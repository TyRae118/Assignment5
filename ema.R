ema <- function (data, period) 
{
    if (period < 1 || period != as.integer(period)) {
        stop("Period must be a positive integer")
    }
    if (length(data) == 0) {
        stop("Data cannot be empty")
    }
    multiplier <- 2/(period + 1)
    ema_values <- numeric(length(data))
    ema_values[1] <- data[1]
    if (length(data) > 1) {
        for (i in 2:length(data)) {
            ema_values[i] <- (data[i] - ema_values[i - 1]) * multiplier + ema_values[i - 1]
        }
    }
    return(ema_values)
}
