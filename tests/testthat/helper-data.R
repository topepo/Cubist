# Constructor functions for test data

#' Create numeric test data for cubist models
#' @param n Number of observations
#' @param p Number of predictors
#' @param seed Random seed
#' @return List with x (data.frame) and y (numeric vector)
new_cubist_data <- function(n = 100, p = 5, seed = 123) {
  set.seed(seed)
  x <- as.data.frame(matrix(rnorm(n * p), n, p))
  names(x) <- paste0("x", seq_len(p))
  y <- rowSums(x) + rnorm(n, sd = 0.1)
  list(x = x, y = y)
}

#' Create mixed-type test data
#' @param n Number of observations
#' @param seed Random seed
#' @return data.frame with numeric, factor, ordered, and character columns
new_mixed_data <- function(n = 100, seed = 123) {
  set.seed(seed)
  data.frame(
    num = rnorm(n),
    fac = factor(sample(letters[1:3], n, replace = TRUE)),
    ord = ordered(
      sample(c("low", "med", "high"), n, replace = TRUE),
      levels = c("low", "med", "high")
    ),
    chr = sample(c("a", "b"), n, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

#' Fit a simple model for testing predictions
#' @param n Number of observations
#' @param committees Number of committees
#' @param seed Random seed
#' @return A cubist model object
new_fitted_model <- function(n = 100, committees = 1, seed = 123) {
  data <- new_cubist_data(n = n, seed = seed)
  cubist(data$x, data$y, committees = committees)
}

#' Create test data with a "sample" column (reserved name)
#' @param n Number of observations
#' @param seed Random seed
#' @return List with x and y
new_sample_name_data <- function(n = 50, seed = 123) {
  set.seed(seed)
  x <- data.frame(
    sample = rnorm(n),
    other = rnorm(n)
  )
  y <- x$sample + x$other + rnorm(n, sd = 0.1)
  list(x = x, y = y)
}
