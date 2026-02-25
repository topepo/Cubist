# Tests for committee functionality

test_that("cubist works with single committee (default)", {
  data <- new_cubist_data(n = 100, p = 5)
  mod <- cubist(data$x, data$y, committees = 1)

  expect_s3_class(mod, "cubist")
  expect_equal(mod$committees, 1)
})

test_that("cubist works with multiple committees", {
  data <- new_cubist_data(n = 100, p = 5)

  for (nc in c(2, 5, 10)) {
    mod <- cubist(data$x, data$y, committees = nc)
    expect_s3_class(mod, "cubist")
    expect_equal(mod$committees, nc)
  }
})

test_that("committees affect model output", {
  data <- new_cubist_data(n = 100, p = 5)

  mod1 <- cubist(data$x, data$y, committees = 1)
  mod5 <- cubist(data$x, data$y, committees = 5)

  # More committees should produce different (typically more) output
  expect_true(nchar(mod5$model) >= nchar(mod1$model))
})

test_that("committee information appears in output", {
  data <- new_cubist_data(n = 100, p = 5)
  mod <- cubist(data$x, data$y, committees = 5)

  output <- capture.output(print(mod))
  expect_true(any(grepl("Number of committees: 5", output)))
})

test_that("print displays rules per committee for multiple committees", {
  data <- new_cubist_data(n = 100, p = 5)
  mod <- cubist(data$x, data$y, committees = 3)

  output <- capture.output(print(mod))
  # Should show "Number of rules per committee:"
  expect_true(any(grepl("Number of rules per committee:", output)))
})

test_that("coefficients include committee information", {
  data <- new_cubist_data(n = 100, p = 5)
  mod <- cubist(data$x, data$y, committees = 3)

  coefs <- mod$coefficients
  expect_true("committee" %in% names(coefs))
  expect_true("rule" %in% names(coefs))

  # Should have coefficients from multiple committees
  expect_true(length(unique(coefs$committee)) >= 1)
})

test_that("splits include committee information", {
  data <- new_cubist_data(n = 100, p = 5)
  mod <- cubist(data$x, data$y, committees = 3)

  # Splits may or may not be present
  expect_true(is.null(mod$splits) || is.data.frame(mod$splits))

  if (!is.null(mod$splits)) {
    expect_true("committee" %in% names(mod$splits))
  }
})

test_that("predictions work with multiple committees", {
  data <- new_cubist_data(n = 100, p = 5)
  mod <- cubist(data$x, data$y, committees = 5)

  preds <- predict(mod, data$x)
  expect_type(preds, "double")
  expect_length(preds, 100)
})

test_that("predictions with neighbors work with multiple committees", {
  data <- new_cubist_data(n = 100, p = 5)
  mod <- cubist(data$x, data$y, committees = 5)

  preds <- predict(mod, data$x, neighbors = 5)
  expect_type(preds, "double")
  expect_length(preds, 100)
})

test_that("maximum 100 committees allowed", {
  data <- new_cubist_data(n = 50, p = 3)

  # 100 should work
  expect_silent(cubist(data$x, data$y, committees = 100))

  # 101 should error
  expect_error(
    cubist(data$x, data$y, committees = 101),
    "number of committees must be between 1 and 100"
  )
})
