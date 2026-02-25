# Tests for R/lattice.R (dotplot.cubist)

test_that("dotplot.cubist returns trellis object for splits", {
  skip_if_not_installed("mlbench")

  library(mlbench)
  data(BostonHousing)

  mod <- cubist(x = BostonHousing[, -14], y = BostonHousing$medv)

  if (!is.null(mod$splits)) {
    plt <- dotplot(mod, what = "splits")
    expect_s3_class(plt, "trellis")
  }
})

test_that("dotplot.cubist returns trellis object for coefs", {
  skip_if_not_installed("mlbench")

  library(mlbench)
  data(BostonHousing)

  mod <- cubist(x = BostonHousing[, -14], y = BostonHousing$medv)

  plt <- dotplot(mod, what = "coefs")
  expect_s3_class(plt, "trellis")
})

test_that("dotplot.cubist errors when no splits in model", {
  # Create a model that might have no splits
  set.seed(42)
  x <- data.frame(x1 = rnorm(30))
  y <- x$x1 + rnorm(30, sd = 0.01)

  mod <- cubist(x, y)

  # Manually set splits to NULL for testing
  mod_no_splits <- mod
  mod_no_splits$splits <- NULL

  expect_error(
    dotplot(mod_no_splits),
    "No splits were used in this model"
  )
})

test_that("dotplot.cubist filters by committee", {
  skip_if_not_installed("mlbench")

  library(mlbench)
  data(BostonHousing)

  mod <- cubist(
    x = BostonHousing[, -14],
    y = BostonHousing$medv,
    committees = 5
  )

  if (!is.null(mod$splits)) {
    plt <- dotplot(mod, what = "splits", committee = 2)
    expect_s3_class(plt, "trellis")
  }
})

test_that("dotplot.cubist filters by rule", {
  skip_if_not_installed("mlbench")

  library(mlbench)
  data(BostonHousing)

  mod <- cubist(x = BostonHousing[, -14], y = BostonHousing$medv)

  if (!is.null(mod$splits)) {
    plt <- dotplot(mod, what = "splits", rule = 2)
    expect_s3_class(plt, "trellis")
  }
})

test_that("dotplot.cubist creates labels for single committee", {
  skip_if_not_installed("mlbench")

  library(mlbench)
  data(BostonHousing)

  mod <- cubist(
    x = BostonHousing[, -14],
    y = BostonHousing$medv,
    committees = 1
  )

  if (!is.null(mod$splits)) {
    plt <- dotplot(mod, what = "splits")
    expect_s3_class(plt, "trellis")
  }
})

test_that("dotplot.cubist creates labels for multiple committees", {
  skip_if_not_installed("mlbench")

  library(mlbench)
  data(BostonHousing)

  mod <- cubist(
    x = BostonHousing[, -14],
    y = BostonHousing$medv,
    committees = 3
  )

  if (!is.null(mod$splits)) {
    plt <- dotplot(mod, what = "splits")
    expect_s3_class(plt, "trellis")
  }
})

test_that("dotplot.cubist coefs works with multiple committees", {
  skip_if_not_installed("mlbench")

  library(mlbench)
  data(BostonHousing)

  mod <- cubist(
    x = BostonHousing[, -14],
    y = BostonHousing$medv,
    committees = 3
  )

  plt <- dotplot(mod, what = "coefs")
  expect_s3_class(plt, "trellis")
})

test_that("dotplot.cubist errors with only categorical splits", {
  # Create data with only categorical predictor
  set.seed(123)
  x <- data.frame(
    fac = factor(sample(letters[1:5], 100, replace = TRUE))
  )
  y <- as.numeric(x$fac) + rnorm(100, sd = 0.5)

  mod <- cubist(x, y)

  # If all splits are type3, dotplot for splits should error
  if (!is.null(mod$splits) && all(mod$splits$type == "type3")) {
    expect_error(
      dotplot(mod, what = "splits"),
      "No splits of continuous predictors were made"
    )
  }
})

test_that("dotplot passes additional arguments to lattice", {
  skip_if_not_installed("mlbench")

  library(mlbench)
  data(BostonHousing)

  mod <- cubist(x = BostonHousing[, -14], y = BostonHousing$medv)

  # Pass scales argument
  plt <- dotplot(
    mod,
    what = "coefs",
    scales = list(x = list(relation = "free"))
  )
  expect_s3_class(plt, "trellis")
})

test_that("dotplot panel function renders correctly for splits", {
  skip_if_not_installed("mlbench")

  library(mlbench)
  data(BostonHousing)

  mod <- cubist(x = BostonHousing[, -14], y = BostonHousing$medv)

  if (!is.null(mod$splits) && any(mod$splits$type == "type2")) {
    plt <- dotplot(mod, what = "splits")

    # Actually render the plot to exercise panel function
    pdf(tempfile())
    print(plt)
    dev.off()

    expect_s3_class(plt, "trellis")
  }
})

test_that("dotplot panel function renders with both upper and lower splits", {
  skip_if_not_installed("mlbench")

  library(mlbench)
  data(BostonHousing)

  # Use multiple committees to get more rules/splits
  mod <- cubist(
    x = BostonHousing[, -14],
    y = BostonHousing$medv,
    committees = 5
  )

  if (!is.null(mod$splits) && any(mod$splits$type == "type2")) {
    plt <- dotplot(mod, what = "splits")

    # Render the plot
    pdf(tempfile())
    print(plt)
    dev.off()

    expect_s3_class(plt, "trellis")
  }
})

test_that("dotplot coefs panel renders correctly", {
  skip_if_not_installed("mlbench")

  library(mlbench)
  data(BostonHousing)

  mod <- cubist(x = BostonHousing[, -14], y = BostonHousing$medv)

  plt <- dotplot(mod, what = "coefs")

  # Actually render the plot
  pdf(tempfile())
  print(plt)
  dev.off()

  expect_s3_class(plt, "trellis")
})
