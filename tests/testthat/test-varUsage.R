# Tests for R/varUsage.R

test_that("varUsage parses attribute usage from output", {
  data <- new_cubist_data(n = 100, p = 5)
  mod <- cubist(data$x, data$y)

  usage <- Cubist:::varUsage(mod$output)

  if (!is.null(usage)) {
    expect_s3_class(usage, "data.frame")
    expect_true("Conditions" %in% names(usage))
    expect_true("Model" %in% names(usage))
    expect_true("Variable" %in% names(usage))
  }
})

test_that("varUsage returns data.frame with correct columns", {
  data <- new_cubist_data(n = 100, p = 5)
  mod <- cubist(data$x, data$y)

  usage <- mod$usage

  expect_s3_class(usage, "data.frame")
  expect_named(usage, c("Conditions", "Model", "Variable"))
})

test_that("varUsage handles conditions-only percentages", {
  # This would be a case where a variable is used only in conditions
  data <- new_cubist_data(n = 100, p = 5)
  mod <- cubist(data$x, data$y)

  usage <- mod$usage

  # Some variables might have Conditions > 0 and Model = 0
  expect_s3_class(usage, "data.frame")
  expect_all_true(usage$Conditions >= 0)
  expect_all_true(usage$Model >= 0)
})

test_that("varUsage handles model-only percentages", {
  # Variables used only in model, not conditions
  data <- new_cubist_data(n = 100, p = 5)
  mod <- cubist(data$x, data$y)

  usage <- mod$usage

  # Some variables might have Model > 0 and Conditions = 0
  expect_s3_class(usage, "data.frame")
})

test_that("varUsage extracts variable names correctly", {
  data <- new_cubist_data(n = 100, p = 5)
  mod <- cubist(data$x, data$y)

  usage <- mod$usage

  # Variable names should match predictor names
  expect_all_true(names(data$x) %in% usage$Variable)
})

test_that("varUsage handles unused variables", {
  data <- new_cubist_data(n = 100, p = 10)
  mod <- cubist(data$x, data$y)

  usage <- mod$usage

  # All variables should be in usage (even if not used)
  expect_all_true(names(data$x) %in% usage$Variable)

  # Unused variables should have 0 for both
  unused <- usage[usage$Conditions == 0 & usage$Model == 0, ]
  # It's possible to have some or none unused
  expect_s3_class(unused, "data.frame")
})

test_that("varUsage works with multiple committees", {
  data <- new_cubist_data(n = 100, p = 5)
  mod <- cubist(data$x, data$y, committees = 5)

  usage <- mod$usage

  expect_s3_class(usage, "data.frame")
  expect_true(nrow(usage) > 0)
})

test_that("cubist model stores usage information", {
  data <- new_cubist_data(n = 100, p = 5)
  mod <- cubist(data$x, data$y)

  expect_true("usage" %in% names(mod))
  expect_s3_class(mod$usage, "data.frame")
})

test_that("varUsage error with invalid output", {
  # Create invalid output string that doesn't have attribute usage
  expect_snapshot(
    Cubist:::varUsage("No attribute usage here"),
    error = TRUE
  )
})
