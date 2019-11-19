alpha <- function(std_errors){
  # Alpha for a given estimate is:
  # the square root of that estimate's standard error over the sum of the square roots of all the standard errors
  std_errors_sqrt <- sapply(std_errors, sqrt)
  std_errors_sqrt_sum <- sum(std_errors_sqrt)

  output <- std_errors_sqrt / std_errors_sqrt_sum

  # output <- setNames(output, std_error_names)

  return(output)
}

std_dev <- function(values) {
  mean <- mean(values)
  n <- length(values)
  sqrt(sum((values - mean)^2) / n)
}


mean_ht <- function(weights,
                    values){
  # First get the standard error
  N <- length(values)
  standard_deviation <- std_dev(values)
  standard_error <- standard_deviation / sqrt(N)

  # Then we find the weighted mean
  weighted_sum <- sum(weights * values)
  # Note that the sum of the weights should equal the size of the inference area!
  weight_sum <- sum(weights)

  mean <- weighted_sum / weight_sum

  return("mean" = mean,
         "standard_error" = standard_error)
}

total_ht <- function(weights,
                    values){
  # First get the standard error
  N <- length(values)
  standard_deviation <- std_dev(values)
  standard_error <- standard_deviation / sqrt(N)

  # Then we find the weighted sum
  weighted_sum <- sum(weights * values)

  return("total" = weighted_sum,
         "standard_error" = standard_error)
}

mean_combined <- function(weights,
                          values,
                          alphas = NULL) {
  ht_estimates <- mapply(weight_vec = weights,
                         value_vec = values,
                         FUN = function(weights, values){
                           mean_ht(weights = weight_vec,
                                   values = value_vec)
                         })
  estimates <- sapply(X = ht_estimates,
                            FUN = function(X){
                              X[[1]]
                            })
  standard_errors <- sapply(X = ht_estimates,
                            FUN = function(X){
                              X[["standard_error"]]
                            })

  # In case these weren't set manually
  if (is.null(alphas)) {
    alphas <- alpha(std_errors = standard_errors)
  }


  output <- sum(alphas * estimates)

  return(output)
}
