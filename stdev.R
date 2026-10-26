stdev <- function (data) 
{
    if (length(data) == 0) {
        stop("Data cannot be empty")
    }
    mean_value <- sum(data)/length(data)
    differences <- data - mean_value
    squared_differences <- differences^2
    variance <- sum(squared_differences)/length(data)
    return(sqrt(variance))
}
