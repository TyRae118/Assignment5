rsi <- function (data, period) 
{
    if (period < 1 || period != as.integer(period)) {
        stop("Period must be a positive integer")
    }
    if (length(data) <= period) {
        stop("Data must contain more values than the period")
    }
    changes <- diff(data)
    gains <- pmax(changes, 0)
    losses <- pmax(-changes, 0)
    avg_gain <- sum(gains[1:period])/period
    avg_loss <- sum(losses[1:period])/period
    rsi_values <- rep(NA_real_, length(data))
    calculate_rsi <- function(gain, loss) {
        if (gain == 0 && loss == 0) 
            return(50)
        if (loss == 0) 
            return(100)
        if (gain == 0) 
            return(0)
        rs <- gain/loss
        return(100 - (100/(1 + rs)))
    }
    rsi_values[period + 1] <- calculate_rsi(avg_gain, avg_loss)
    if (length(data) > period + 1) {
        for (i in (period + 2):length(data)) {
            avg_gain <- (avg_gain * (period - 1) + gains[i - 1])/period
            avg_loss <- (avg_loss * (period - 1) + losses[i - 1])/period
            rsi_values[i] <- calculate_rsi(avg_gain, avg_loss)
        }
    }
    return(rsi_values)
}
