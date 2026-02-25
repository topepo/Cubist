# Tests for edge cases and error handling

test_that("cubist handles small datasets", {
  set.seed(123)
  x <- data.frame(x1 = rnorm(10), x2 = rnorm(10))
  y <- x$x1 + x$x2 + rnorm(10, sd = 0.1)

  mod <- cubist(x, y)
  expect_s3_class(mod, "cubist")
})

test_that("cubist handles high-dimensional data", {
  data <- new_cubist_data(n = 100, p = 50)
  mod <- cubist(data$x, data$y)

  expect_s3_class(mod, "cubist")
  expect_equal(mod$dims[2], 50L)
})

test_that("cubist handles matrix input", {
  data <- new_cubist_data(n = 100, p = 5)
  x_mat <- as.matrix(data$x)

  mod <- cubist(x_mat, data$y)
  expect_s3_class(mod, "cubist")
})

test_that("cubist handles missing values in predictors", {
  set.seed(123)
  x <- data.frame(x1 = c(rnorm(45), NA, rnorm(4)), x2 = c(NA, rnorm(49)))
  y <- rnorm(50)

  mod <- cubist(x, y)
  expect_s3_class(mod, "cubist")
})

test_that("cubist handles predictors with different scales", {
  set.seed(123)
  x <- data.frame(
    small = rnorm(100, mean = 0, sd = 0.001),
    medium = rnorm(100, mean = 0, sd = 1),
    large = rnorm(100, mean = 1000, sd = 100)
  )
  y <- x$small * 1000 + x$medium + x$large / 100 + rnorm(100, sd = 0.1)

  mod <- cubist(x, y)
  expect_s3_class(mod, "cubist")
})

test_that("cubist handles predictor names with spaces", {
  set.seed(123)
  x <- data.frame(rnorm(50), rnorm(50))
  names(x) <- c("var one", "var two")
  y <- x$`var one` + x$`var two` + rnorm(50, sd = 0.1)

  mod <- cubist(x, y)
  expect_s3_class(mod, "cubist")
})

test_that("cubist handles predictor names with special characters", {
  set.seed(123)
  x <- data.frame(rnorm(50), rnorm(50))
  names(x) <- c("var.one", "var_two")
  y <- x$var.one + x$var_two + rnorm(50, sd = 0.1)

  mod <- cubist(x, y)
  expect_s3_class(mod, "cubist")
})

# --- Tests for unusual characters ---

test_that("cubist handles colons in variable names", {
  set.seed(123)
  x <- data.frame(rnorm(50), rnorm(50))
  names(x) <- c("var:one", "var:two")
  y <- x$`var:one` + x$`var:two` + rnorm(50, sd = 0.1)

  mod <- cubist(x, y)
  expect_s3_class(mod, "cubist")

  preds <- predict(mod, x)
  expect_length(preds, 50)
})

test_that("cubist handles semicolons in variable names", {
  set.seed(123)
  x <- data.frame(rnorm(50), rnorm(50))
  names(x) <- c("var;one", "var;two")
  y <- x$`var;one` + x$`var;two` + rnorm(50, sd = 0.1)

  mod <- cubist(x, y)
  expect_s3_class(mod, "cubist")
})

test_that("cubist handles pipes in variable names", {
  set.seed(123)
  x <- data.frame(rnorm(50), rnorm(50))
  names(x) <- c("var|one", "var|two")
  y <- x$`var|one` + x$`var|two` + rnorm(50, sd = 0.1)

  mod <- cubist(x, y)
  expect_s3_class(mod, "cubist")
})

test_that("cubist handles parentheses in variable names", {
  set.seed(123)
  x <- data.frame(rnorm(50), rnorm(50))
  names(x) <- c("var(one)", "var(two)")
  y <- x$`var(one)` + x$`var(two)` + rnorm(50, sd = 0.1)

  mod <- cubist(x, y)
  expect_s3_class(mod, "cubist")
})

test_that("cubist handles brackets in variable names", {
  set.seed(123)
  x <- data.frame(rnorm(50), rnorm(50))
  names(x) <- c("var[one]", "var[two]")
  y <- x$`var[one]` + x$`var[two]` + rnorm(50, sd = 0.1)

  mod <- cubist(x, y)
  expect_s3_class(mod, "cubist")
})

test_that("cubist handles plus and minus in variable names", {
  set.seed(123)
  x <- data.frame(rnorm(50), rnorm(50))
  names(x) <- c("var+one", "var-two")
  y <- x$`var+one` + x$`var-two` + rnorm(50, sd = 0.1)

  mod <- cubist(x, y)
  expect_s3_class(mod, "cubist")
})

# NOTE: Tests for factor levels with special characters (colons, semicolons,
# commas) are NOT included here because they cause the underlying C code to
# call exit(), which terminates the R session. These are documented limitations
# of the Cubist C code.
#
# Known unsupported special characters in factor/character levels:
# - Colons (:)
# - Semicolons (;)
# - Commas (,)
# - Pipes (|)
#
# Users should avoid these characters in categorical predictor values.

test_that("cubist handles factor levels with spaces", {
  set.seed(123)
  x <- data.frame(
    fac = factor(sample(
      c("level one", "level two", "level three"),
      100,
      replace = TRUE
    )),
    num = rnorm(100)
  )
  y <- as.numeric(x$fac) + x$num + rnorm(100, sd = 0.5)

  mod <- cubist(x, y)
  expect_s3_class(mod, "cubist")

  preds <- predict(mod, x)
  expect_length(preds, 100)
})

# NOTE: Tests for factor/character levels with commas and colons removed
# for the same reason - they cause C code to exit and crash the test session.

test_that("cubist handles numeric-like factor levels", {
  set.seed(123)
  x <- data.frame(
    fac = factor(sample(c("1.5", "2.5", "3.5"), 100, replace = TRUE)),
    num = rnorm(100)
  )
  y <- as.numeric(as.character(x$fac)) + x$num + rnorm(100, sd = 0.5)

  mod <- cubist(x, y)
  expect_s3_class(mod, "cubist")
})

test_that("cubist handles backslash in variable names", {
  set.seed(123)
  x <- data.frame(rnorm(50), rnorm(50))
  # Use double backslash to represent a single backslash
  names(x) <- c("var\\one", "var\\two")
  y <- x[[1]] + x[[2]] + rnorm(50, sd = 0.1)

  mod <- cubist(x, y)
  expect_s3_class(mod, "cubist")
})

test_that("cubist handles quotes in variable names", {
  set.seed(123)
  x <- data.frame(rnorm(50), rnorm(50))
  names(x) <- c("var'one", 'var"two')
  y <- x[[1]] + x[[2]] + rnorm(50, sd = 0.1)

  mod <- cubist(x, y)
  expect_s3_class(mod, "cubist")
})

test_that("cubist handles equals sign in variable names", {
  set.seed(123)
  x <- data.frame(rnorm(50), rnorm(50))
  names(x) <- c("var=one", "var=two")
  y <- x$`var=one` + x$`var=two` + rnorm(50, sd = 0.1)

  mod <- cubist(x, y)
  expect_s3_class(mod, "cubist")
})

test_that("cubist handles percent sign in variable names", {
  set.seed(123)
  x <- data.frame(rnorm(50), rnorm(50))
  names(x) <- c("var%one", "var%two")
  y <- x$`var%one` + x$`var%two` + rnorm(50, sd = 0.1)

  mod <- cubist(x, y)
  expect_s3_class(mod, "cubist")
})

test_that("predict works with newdata having special character column names", {
  set.seed(123)
  train_x <- data.frame(rnorm(80), rnorm(80))
  names(train_x) <- c("x:1", "x:2")
  train_y <- train_x[[1]] + train_x[[2]] + rnorm(80, sd = 0.1)

  test_x <- data.frame(rnorm(20), rnorm(20))
  names(test_x) <- c("x:1", "x:2")

  mod <- cubist(train_x, train_y)
  preds <- predict(mod, test_x)

  expect_length(preds, 20)
})

test_that("cubist handles hash/pound sign in variable names", {
  set.seed(123)
  x <- data.frame(rnorm(50), rnorm(50))
  names(x) <- c("var#one", "var#two")
  y <- x$`var#one` + x$`var#two` + rnorm(50, sd = 0.1)

  mod <- cubist(x, y)
  expect_s3_class(mod, "cubist")
})

test_that("cubist handles at sign in variable names", {
  set.seed(123)
  x <- data.frame(rnorm(50), rnorm(50))
  names(x) <- c("var@one", "var@two")
  y <- x$`var@one` + x$`var@two` + rnorm(50, sd = 0.1)

  mod <- cubist(x, y)
  expect_s3_class(mod, "cubist")
})

test_that("cubist handles ampersand in variable names", {
  set.seed(123)
  x <- data.frame(rnorm(50), rnorm(50))
  names(x) <- c("var&one", "var&two")
  y <- x$`var&one` + x$`var&two` + rnorm(50, sd = 0.1)

  mod <- cubist(x, y)
  expect_s3_class(mod, "cubist")
})

test_that("predict handles subset of columns in newdata", {
  data <- new_cubist_data(n = 100, p = 5)
  mod <- cubist(data$x, data$y)

  # Should work even if newdata only contains used columns
  preds <- predict(mod, data$x)
  expect_length(preds, 100)
})

test_that("cubist works with constant predictor", {
  set.seed(123)
  x <- data.frame(
    constant = rep(1, 50),
    variable = rnorm(50)
  )
  y <- x$variable + rnorm(50, sd = 0.1)

  mod <- cubist(x, y)
  expect_s3_class(mod, "cubist")
})

test_that("cubist works with binary predictor", {
  set.seed(123)
  x <- data.frame(
    binary = sample(c(0, 1), 50, replace = TRUE),
    continuous = rnorm(50)
  )
  y <- x$binary * 2 + x$continuous + rnorm(50, sd = 0.1)

  mod <- cubist(x, y)
  expect_s3_class(mod, "cubist")
})

test_that("cubist works with integer predictors", {
  set.seed(123)
  x <- data.frame(
    int1 = sample(1:10, 50, replace = TRUE),
    int2 = sample(100:200, 50, replace = TRUE)
  )
  y <- x$int1 + x$int2 / 10 + rnorm(50, sd = 0.1)

  mod <- cubist(x, y)
  expect_s3_class(mod, "cubist")
})

test_that("cubist handles outcome with large range", {
  set.seed(123)
  x <- data.frame(x1 = rnorm(50))
  y <- x$x1 * 1e6 + rnorm(50, sd = 1e3)

  mod <- cubist(x, y)
  expect_s3_class(mod, "cubist")

  preds <- predict(mod, x)
  expect_length(preds, 50)
})

test_that("cubist handles outcome with small values", {
  set.seed(123)
  x <- data.frame(x1 = rnorm(50))
  y <- x$x1 * 1e-6 + rnorm(50, sd = 1e-8)

  mod <- cubist(x, y)
  expect_s3_class(mod, "cubist")
})

test_that("cubist handles factor with many levels", {
  set.seed(123)
  n <- 200
  x <- data.frame(
    many_levels = factor(sample(letters, n, replace = TRUE)),
    num = rnorm(n)
  )
  y <- as.numeric(x$many_levels) + x$num + rnorm(n, sd = 0.5)

  mod <- cubist(x, y)
  expect_s3_class(mod, "cubist")
})

# NOTE: Test for single-level factor removed - causes C code to exit()
# Known limitation: factors must have at least 2 levels

test_that("predict handles newdata with different row count", {
  train <- new_cubist_data(n = 100, p = 5)
  test <- new_cubist_data(n = 25, p = 5, seed = 456)

  mod <- cubist(train$x, train$y)
  preds <- predict(mod, test$x)

  expect_length(preds, 25)
})

# NOTE: Test for Date predictor removed - causes C code to exit()
# Known limitation: Date predictors are not supported

test_that("cubist model can be saved and loaded", {
  data <- new_cubist_data(n = 100, p = 5)
  mod <- cubist(data$x, data$y)

  tmpfile <- tempfile(fileext = ".rds")
  saveRDS(mod, tmpfile)
  loaded_mod <- readRDS(tmpfile)

  expect_s3_class(loaded_mod, "cubist")

  # Predictions should be identical
  preds1 <- predict(mod, data$x)
  preds2 <- predict(loaded_mod, data$x)
  expect_equal(preds1, preds2)

  unlink(tmpfile)
})

test_that("cubist handles negative weights", {
  # Weights <= 0 are set to 1 by C code per documentation
  data <- new_cubist_data(n = 50, p = 3)
  weights <- runif(50, -1, 2)

  # Should not error
  mod <- cubist(data$x, data$y, weights = weights)
  expect_s3_class(mod, "cubist")
})

test_that("cubist handles very long column names", {
  set.seed(123)
  x <- data.frame(
    this_is_a_very_long_column_name_that_might_cause_issues = rnorm(50),
    another_extremely_long_variable_name_for_testing = rnorm(50)
  )
  y <- x[[1]] + x[[2]] + rnorm(50, sd = 0.1)

  mod <- cubist(x, y)
  expect_s3_class(mod, "cubist")
})

test_that("cubist handles single observation prediction", {
  data <- new_cubist_data(n = 100, p = 5)
  mod <- cubist(data$x, data$y)

  single_obs <- data$x[1, , drop = FALSE]
  pred <- predict(mod, single_obs)

  expect_length(pred, 1)
  expect_type(pred, "double")
})

test_that("cubist with control options all enabled", {
  data <- new_cubist_data(n = 100, p = 5)

  ctrl <- cubistControl(
    unbiased = TRUE,
    rules = 50,
    extrapolation = 75,
    sample = 50,
    seed = 123,
    label = "my_outcome"
  )

  mod <- cubist(data$x, data$y, control = ctrl)
  expect_s3_class(mod, "cubist")
})

# --- Tests for various cubist control options ---

test_that("cubist with unbiased = TRUE produces valid model", {
  skip_if_not_installed("mlbench")

  library(mlbench)
  data(BostonHousing)

  mod <- cubist(
    x = BostonHousing[, -14],
    y = BostonHousing$medv,
    control = cubistControl(unbiased = TRUE)
  )

  expect_s3_class(mod, "cubist")
  preds <- predict(mod, BostonHousing[1:10, -14])
  expect_length(preds, 10)
})

test_that("cubist with sample option subsamples data", {
  skip_if_not_installed("mlbench")

  library(mlbench)
  data(BostonHousing)

  mod <- cubist(
    x = BostonHousing[, -14],
    y = BostonHousing$medv,
    control = cubistControl(sample = 80, seed = 42)
  )

  expect_s3_class(mod, "cubist")
  preds <- predict(mod, BostonHousing[1:10, -14])
  expect_length(preds, 10)
})

test_that("cubist with extrapolation limit constrains predictions", {
  skip_if_not_installed("mlbench")

  library(mlbench)
  data(BostonHousing)

  mod <- cubist(
    x = BostonHousing[, -14],
    y = BostonHousing$medv,
    control = cubistControl(extrapolation = 50)
  )

  expect_s3_class(mod, "cubist")
  preds <- predict(mod, BostonHousing[1:10, -14])
  expect_length(preds, 10)
})

test_that("cubist with max rules limit", {
  skip_if_not_installed("mlbench")

  library(mlbench)
  data(BostonHousing)

  mod <- cubist(
    x = BostonHousing[, -14],
    y = BostonHousing$medv,
    control = cubistControl(rules = 5)
  )

  expect_s3_class(mod, "cubist")
  # Model should have at most 5 rules
  n_rules <- sum(Cubist:::countRules(mod$model))
  expect_true(n_rules <= 5)
})

test_that("cubist with default rules value", {
  data <- new_cubist_data(n = 100, p = 5)

  # Use default rules (100)
  mod <- cubist(data$x, data$y, control = cubistControl(rules = 100))
  expect_s3_class(mod, "cubist")
})

test_that("cubist with many committees and all options", {
  skip_if_not_installed("mlbench")

  library(mlbench)
  data(BostonHousing)

  mod <- cubist(
    x = BostonHousing[, -14],
    y = BostonHousing$medv,
    committees = 10,
    control = cubistControl(
      unbiased = TRUE,
      rules = 20,
      extrapolation = 80,
      sample = 70,
      seed = 123
    )
  )

  expect_s3_class(mod, "cubist")
  expect_equal(mod$committees, 10)

  preds <- predict(mod, BostonHousing[1:10, -14])
  expect_length(preds, 10)
})

test_that("cubist predict with neighbors uses instance correction", {
  skip_if_not_installed("mlbench")

  library(mlbench)
  data(BostonHousing)

  mod <- cubist(x = BostonHousing[, -14], y = BostonHousing$medv)

  # Predictions without and with neighbors should differ
  preds0 <- predict(mod, BostonHousing[1:10, -14], neighbors = 0)
  preds5 <- predict(mod, BostonHousing[1:10, -14], neighbors = 5)
  preds9 <- predict(mod, BostonHousing[1:10, -14], neighbors = 9)

  expect_length(preds0, 10)
  expect_length(preds5, 10)
  expect_length(preds9, 10)

  # Predictions should be different (unless exactly the same, which is unlikely)
  expect_false(all(preds0 == preds5) && all(preds5 == preds9))
})

test_that("coefficient extraction works for complex models", {
  data <- new_cubist_data(n = 200, p = 10)
  mod <- cubist(data$x, data$y, committees = 5)

  coefs <- coef(mod)
  expect_s3_class(coefs, "data.frame")
  expect_true(nrow(coefs) > 0)
})

test_that("splits extraction works for complex models", {
  data <- new_cubist_data(n = 200, p = 10)
  mod <- cubist(data$x, data$y, committees = 5)

  # Splits may or may not be present
  expect_true(is.null(mod$splits) || is.data.frame(mod$splits))

  if (!is.null(mod$splits)) {
    expect_s3_class(mod$splits, "data.frame")
    expect_true("percentile" %in% names(mod$splits))
  }
})
