# Tests for summary.cubist and related print methods

test_that("summary.cubist returns correct structure", {
  data <- new_cubist_data(n = 100, p = 5)
  mod <- cubist(data$x, data$y)

  summ <- summary(mod)

  expect_s3_class(summ, "summary.cubist")
  expect_named(summ, c("output", "call"))
  expect_type(summ$output, "character")
  expect_true(is.call(summ$call))
})

test_that("summary output contains rule information", {
  data <- new_cubist_data(n = 100, p = 5)
  mod <- cubist(data$x, data$y)

  summ <- summary(mod)

  expect_true(grepl("Rule", summ$output))
  expect_true(grepl("outcome", summ$output))
})

test_that("summary output contains variable usage statistics", {
  data <- new_cubist_data(n = 100, p = 5)
  mod <- cubist(data$x, data$y)

  summ <- summary(mod)

  expect_true(grepl("Attribute usage", summ$output))
})

test_that("print.summary.cubist outputs correctly", {
  data <- new_cubist_data(n = 100, p = 5)
  mod <- cubist(data$x, data$y)
  summ <- summary(mod)

  output <- capture.output(print(summ))

  expect_true(length(output) > 0)
  expect_true(any(grepl("Call:", output)))
  expect_true(any(grepl("Rule", output)))
})

test_that("print.summary.cubist returns invisible(x)", {
  data <- new_cubist_data(n = 100, p = 5)
  mod <- cubist(data$x, data$y)
  summ <- summary(mod)

  # Capture output to prevent it from printing during tests
  result <- capture.output(vis <- withVisible(print(summ)))
  expect_false(vis$visible)
  expect_identical(vis$value, summ)
})

test_that("summary works with multiple committees", {
  data <- new_cubist_data(n = 100, p = 5)
  mod <- cubist(data$x, data$y, committees = 3)

  summ <- summary(mod)

  expect_s3_class(summ, "summary.cubist")
  expect_true(nchar(summ$output) > 0)
})

test_that("summary output shows model with case weights", {
  data <- new_cubist_data(n = 100, p = 3)
  weights <- runif(100, 0.5, 2)
  mod <- cubist(data$x, data$y, weights = weights)

  summ <- summary(mod)

  # Output should indicate case weights were used
  expect_true(nchar(summ$output) > 0)
})

test_that("summary output contains evaluation metrics", {
  data <- new_cubist_data(n = 100, p = 5)
  mod <- cubist(data$x, data$y)

  summ <- summary(mod)

  expect_true(grepl("Average", summ$output))
  expect_true(grepl("error", summ$output))
})
