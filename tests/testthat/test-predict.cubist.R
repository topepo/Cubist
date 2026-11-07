# Tests for R/predict.cubist.R

test_that("predict.cubist works for basic prediction", {
  data <- new_cubist_data(n = 100, p = 5, seed = 123)
  mod <- cubist(data$x, data$y)

  # Predict on training data
  preds <- predict(mod, data$x)

  expect_type(preds, "double")
  expect_length(preds, 100)
  expect_false(any(is.na(preds)))
})

test_that("predict.cubist works with neighbors parameter", {
  data <- new_cubist_data(n = 100, p = 5, seed = 123)
  mod <- cubist(data$x, data$y)

  # Test neighbors from 1 to 9
  for (k in c(1, 5, 9)) {
    preds <- predict(mod, data$x, neighbors = k)
    expect_type(preds, "double")
    expect_length(preds, 100)
  }
})

test_that("predict.cubist reorders columns correctly", {
  data <- new_cubist_data(n = 50, p = 5, seed = 123)
  mod <- cubist(data$x, data$y)

  # Reorder columns in newdata
  newdata <- data$x[, c(3, 1, 5, 2, 4)]
  preds <- predict(mod, newdata)

  expect_type(preds, "double")
  expect_length(preds, 50)
})

test_that("predict.cubist adds case weight column when needed", {
  data <- new_cubist_data(n = 100, p = 3, seed = 123)
  weights <- runif(100, 0.5, 2)
  mod <- cubist(data$x, data$y, weights = weights)

  # Model was trained with weights, predict should work
  preds <- predict(mod, data$x)

  expect_type(preds, "double")
  expect_length(preds, 100)
})

test_that("predict.cubist handles sample parameter in model", {
  data <- new_cubist_data(n = 100, p = 3, seed = 123)
  mod <- cubist(data$x, data$y, control = cubistControl(sample = 50, seed = 42))

  preds <- predict(mod, data$x)

  expect_type(preds, "double")
  expect_length(preds, 100)
})

test_that("predict.cubist errors when newdata is NULL", {
  data <- new_cubist_data(n = 50, p = 3)
  mod <- cubist(data$x, data$y)

  expect_snapshot(
    predict(mod, newdata = NULL),
    error = TRUE
  )
})

test_that("predict.cubist errors with multiple neighbors values", {
  data <- new_cubist_data(n = 50, p = 3)
  mod <- cubist(data$x, data$y)

  expect_snapshot(
    predict(mod, data$x, neighbors = c(1, 2)),
    error = TRUE
  )
})

test_that("predict.cubist errors with invalid neighbors", {
  data <- new_cubist_data(n = 50, p = 3)
  mod <- cubist(data$x, data$y)

  expect_snapshot(
    predict(mod, data$x, neighbors = 51),
    error = TRUE
  )
  expect_snapshot(
    predict(mod, data$x, neighbors = -1),
    error = TRUE
  )
  expect_snapshot(
    predict(mod, data$x, neighbors = 2.5),
    error = TRUE
  )
  expect_snapshot(
    predict(mod, data$x, neighbors = NA),
    error = TRUE
  )
})

test_that("predict.cubist works with more than 9 neighbors", {
  # Rounded predictors produce many tied distances
  set.seed(1)
  x <- data.frame(a = round(rnorm(200), 1), b = round(runif(200), 1))
  y <- 2 * x$a + x$b + rnorm(200, sd = 0.2)
  mod <- cubist(x[1:150, ], y[1:150])
  rule_preds <- predict(mod, x[151:200, ])

  for (k in c(10, 25, 50, 150)) {
    preds <- predict(mod, x[151:200, ], neighbors = k)
    expect_length(preds, 50)
    expect_true(all(is.finite(preds)))
    expect_false(isTRUE(all.equal(preds, rule_preds)))
  }
})

test_that("predict.cubist works on new data with same structure", {
  train <- new_cubist_data(n = 80, p = 5, seed = 123)
  test <- new_cubist_data(n = 20, p = 5, seed = 456)

  mod <- cubist(train$x, train$y)
  preds <- predict(mod, test$x)

  expect_type(preds, "double")
  expect_length(preds, 20)
})

test_that("predict.cubist works with factor predictors", {
  set.seed(3194)
  x_train <- data.frame(
    num = rnorm(80),
    fac = factor(sample(letters[1:3], 80, replace = TRUE))
  )
  y_train <- ifelse(x_train$fac == "a", 1, 2) + x_train$num

  x_test <- data.frame(
    num = rnorm(20),
    fac = factor(
      sample(letters[1:3], 20, replace = TRUE),
      levels = letters[1:3]
    )
  )

  mod <- cubist(x_train, y_train)
  preds <- predict(mod, x_test)

  expect_type(preds, "double")
  expect_length(preds, 20)
})

test_that("neighbors = 0 does not modify model", {
  data <- new_cubist_data(n = 50, p = 3)
  mod <- cubist(data$x, data$y)
  original_model <- mod$model

  preds <- predict(mod, data$x, neighbors = 0)

  # Model should not be modified
  expect_equal(mod$model, original_model)
})

test_that("predict works with tibble newdata", {
  skip_if_not_installed("dplyr")

  data <- new_cubist_data(n = 50, p = 3)
  mod <- cubist(data$x, data$y)

  newdata_tbl <- dplyr::as_tibble(data$x)
  preds <- predict(mod, newdata_tbl)

  expect_type(preds, "double")
  expect_length(preds, 50)
})
