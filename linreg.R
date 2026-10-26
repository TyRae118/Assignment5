linreg <- function (regressionSource, regressionLength, regressionOffset) 
{
    n <- length(regressionSource)
    if (regressionLength > n) {
        stop("regressionLength cannot be greater than the data length")
    }
    if (regressionLength < 2 || regressionLength != as.integer(regressionLength)) {
        stop("regressionLength must be an integer of at least 2")
    }
    if (regressionOffset < 0 || regressionOffset != as.integer(regressionOffset) || regressionOffset >= regressionLength) {
        stop("regressionOffset must be a nonnegative integer less than regressionLength")
    }
    start_index <- max(1, n - regressionLength + regressionOffset)
    end_index <- min(n, n - regressionOffset)
    if (end_index - start_index + 1 < 2) {
        stop("The selected regression window needs at least two values")
    }
    source_subset <- regressionSource[start_index:end_index]
    index_values <- seq_along(source_subset)
    mean_index <- sum(index_values)/length(index_values)
    mean_source <- sum(source_subset)/length(source_subset)
    numerator <- sum((index_values - mean_index) * (source_subset - mean_source))
    denominator <- sum((index_values - mean_index)^2)
    slope <- numerator/denominator
    intercept <- mean_source - slope * mean_index
    predicted_values <- slope * index_values + intercept
    return(list(slope = slope, intercept = intercept, predicted_values = predicted_values))
}
