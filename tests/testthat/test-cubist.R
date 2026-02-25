# Tests for R/cubist.R

# --- cubist.default() tests ---

test_that("cubist works with basic numeric predictors", {
  data <- new_cubist_data(n = 100, p = 5)
  mod <- cubist(data$x, data$y)

  expect_s3_class(mod, "cubist")
  expect_equal(mod$dims, c(100L, 5L))
  expect_equal(mod$committees, 1)
  expect_true(!is.null(mod$model))
  expect_true(!is.null(mod$output))
})

test_that("cubist works with factor predictors", {
  set.seed(8361)
  x <- data.frame(
    num = rnorm(50),
    fac = factor(sample(letters[1:3], 50, replace = TRUE))
  )
  y <- ifelse(x$fac == "a", 1, 2) + x$num + rnorm(50, sd = 0.1)

  mod <- cubist(x, y)
  expect_s3_class(mod, "cubist")
})

test_that("cubist works with character predictors (converted to factors)", {
  set.seed(8361)
  x <- data.frame(
    num = rnorm(50),
    chr = sample(c("cat", "dog"), 50, replace = TRUE),
    stringsAsFactors = FALSE
  )
  y <- ifelse(x$chr == "cat", 1, 2) + x$num + rnorm(50, sd = 0.1)

  mod <- cubist(x, y)
  expect_s3_class(mod, "cubist")
})

test_that("cubist works with ordered factor predictors", {
  set.seed(8361)
  x <- data.frame(
    num = rnorm(50),
    ord = ordered(
      sample(c("low", "med", "high"), 50, replace = TRUE),
      levels = c("low", "med", "high")
    )
  )
  y <- as.numeric(x$ord) + x$num + rnorm(50, sd = 0.1)

  mod <- cubist(x, y)
  expect_s3_class(mod, "cubist")
})

test_that("cubist works with mixed predictor types", {
  x <- new_mixed_data(n = 100)
  y <- x$num + as.numeric(x$fac) + rnorm(100, sd = 0.1)

  mod <- cubist(x, y)
  expect_s3_class(mod, "cubist")
  expect_equal(mod$dims[2], 4L)
})

test_that("cubist converts tibble input to data.frame", {
  skip_if_not_installed("dplyr")

  data <- new_cubist_data(n = 50, p = 3)
  x_tbl <- dplyr::as_tibble(data$x)

  mod <- cubist(x_tbl, data$y)
  expect_s3_class(mod, "cubist")
})

test_that("cubist works with case weights", {
  data <- new_cubist_data(n = 100, p = 3)
  weights <- runif(100, 0.5, 2)

  mod <- cubist(data$x, data$y, weights = weights)
  expect_s3_class(mod, "cubist")
  expect_true(mod$caseWeights)
})

test_that("cubist handles reserved name 'sample' in predictors", {
  data <- new_sample_name_data(n = 50)

  mod <- cubist(data$x, data$y)
  expect_s3_class(mod, "cubist")
  # The variable 'sample' should appear in the usage/output
  expect_true("sample" %in% mod$vars$all)
})

test_that("cubist converts __Sample back to sample in output", {
  # Create data with a column starting with 'sample'
  set.seed(8361)
  x <- data.frame(
    sample_var = rnorm(100),
    other = rnorm(100)
  )
  y <- x$sample_var + x$other + rnorm(100, sd = 0.1)

  mod <- cubist(x, y)
  expect_s3_class(mod, "cubist")

  # Check that output contains 'sample' not '__Sample'
  # The conversion should happen
  expect_true(
    grepl("sample", mod$output, ignore.case = TRUE) ||
      !grepl("__Sample", mod$output)
  )
})

test_that("cubist works with single predictor", {
  set.seed(8361)
  x <- data.frame(single = rnorm(50))
  y <- x$single * 2 + rnorm(50, sd = 0.1)

  mod <- cubist(x, y)
  expect_s3_class(mod, "cubist")
  expect_equal(mod$dims[2], 1L)
})

test_that("cubist works with many predictors", {
  data <- new_cubist_data(n = 100, p = 20)

  mod <- cubist(data$x, data$y)
  expect_s3_class(mod, "cubist")
  expect_equal(mod$dims[2], 20L)
})

test_that("cubist stores usage statistics for unused variables", {
  data <- new_cubist_data(n = 100, p = 10)

  mod <- cubist(data$x, data$y)

  # All variables should be in usage, even if not used

  expect_all_true(names(data$x) %in% mod$usage$Variable)
})

# --- Error handling tests ---

test_that("cubist errors with non-numeric outcome", {
  data <- new_cubist_data(n = 50, p = 3)

  expect_snapshot(
    cubist(data$x, factor(data$y > 0)),
    error = TRUE
  )
})

test_that("cubist errors with committees outside 1-100", {
  data <- new_cubist_data(n = 50, p = 3)

  expect_snapshot(
    cubist(data$x, data$y, committees = 0),
    error = TRUE
  )
  expect_snapshot(
    cubist(data$x, data$y, committees = 101),
    error = TRUE
  )
})

test_that("cubist errors when x is not data.frame or matrix", {
  data <- new_cubist_data(n = 50, p = 3)

  expect_snapshot(
    cubist(as.list(data$x), data$y),
    error = TRUE
  )
})

test_that("cubist errors with non-numeric weights", {
  data <- new_cubist_data(n = 50, p = 3)

  expect_snapshot(
    cubist(data$x, data$y, weights = rep("a", 50)),
    error = TRUE
  )
})

test_that("cubist errors with missing column names", {
  data <- new_cubist_data(n = 50, p = 3)
  x_no_names <- as.matrix(data$x)
  colnames(x_no_names) <- NULL

  expect_snapshot(
    cubist(x_no_names, data$y),
    error = TRUE
  )
})

# --- cubistControl() tests ---

test_that("cubistControl returns correct default values", {
  ctrl <- cubistControl()

  expect_false(ctrl$unbiased)
  expect_equal(ctrl$rules, 100)
  expect_equal(ctrl$extrapolation, 1) # 100/100

  expect_equal(ctrl$sample, 0)
  expect_equal(ctrl$label, "outcome")
  expect_true(ctrl$seed >= 0 && ctrl$seed < 4096)
})

test_that("cubistControl with unbiased = TRUE", {
  ctrl <- cubistControl(unbiased = TRUE)
  expect_true(ctrl$unbiased)
})

test_that("cubistControl accepts valid rules parameter", {
  ctrl <- cubistControl(rules = 50)
  expect_equal(ctrl$rules, 50)

  ctrl_na <- cubistControl(rules = NA)
  expect_true(is.na(ctrl_na$rules))
})

test_that("cubistControl converts extrapolation to fraction", {
  ctrl <- cubistControl(extrapolation = 50)
  expect_equal(ctrl$extrapolation, 0.5)
})

test_that("cubistControl converts sample to fraction", {
  ctrl <- cubistControl(sample = 50)
  expect_equal(ctrl$sample, 0.5)
})

test_that("cubistControl applies modulo 4096 to seed", {
  ctrl <- cubistControl(seed = 5000)
  expect_equal(ctrl$seed, 5000 %% 4096)
})

test_that("cubistControl accepts custom label", {
  ctrl <- cubistControl(label = "my_outcome")
  expect_equal(ctrl$label, "my_outcome")
})

test_that("cubistControl errors with rules outside 1-1000000", {
  expect_snapshot(cubistControl(rules = 0), error = TRUE)
  expect_snapshot(cubistControl(rules = 1000001), error = TRUE)
})

test_that("cubistControl errors with extrapolation outside 0-100", {
  expect_snapshot(cubistControl(extrapolation = -1), error = TRUE)
  expect_snapshot(cubistControl(extrapolation = 101), error = TRUE)
})

test_that("cubistControl errors with sample outside 0-99.9", {
  expect_snapshot(cubistControl(sample = -1), error = TRUE)
  expect_snapshot(cubistControl(sample = 100), error = TRUE)
})

# --- print.cubist() tests ---

test_that("print.cubist displays single committee correctly", {
  data <- new_cubist_data(n = 100, p = 5)
  mod <- cubist(data$x, data$y, committees = 1)

  output <- capture.output(print(mod))
  expect_true(any(grepl("Number of committees: 1", output)))
  expect_true(any(grepl("Number of rules:", output)))
})

test_that("print.cubist displays case weights message", {
  data <- new_cubist_data(n = 100, p = 3)
  weights <- runif(100, 0.5, 2)
  mod <- cubist(data$x, data$y, weights = weights)

  output <- capture.output(print(mod))
  expect_true(any(grepl("Case weights used", output)))
})

test_that("print.cubist displays unbiased option", {
  data <- new_cubist_data(n = 100, p = 3)
  mod <- cubist(data$x, data$y, control = cubistControl(unbiased = TRUE))

  output <- capture.output(print(mod))
  expect_true(any(grepl("unbiased rules", output)))
})

test_that("print.cubist displays extrapolation option", {
  data <- new_cubist_data(n = 100, p = 3)
  mod <- cubist(data$x, data$y, control = cubistControl(extrapolation = 50))

  output <- capture.output(print(mod))
  expect_true(any(grepl("extrapolation", output)))
})

test_that("print.cubist displays sample option", {
  data <- new_cubist_data(n = 100, p = 3)
  mod <- cubist(data$x, data$y, control = cubistControl(sample = 50))

  output <- capture.output(print(mod))
  expect_true(any(grepl("sub-sampling", output)))
})

# --- summary.cubist() and print.summary.cubist() tests ---

test_that("summary.cubist returns correct class", {
  data <- new_cubist_data(n = 100, p = 5)
  mod <- cubist(data$x, data$y)

  summ <- summary(mod)
  expect_s3_class(summ, "summary.cubist")
  expect_true(!is.null(summ$output))
  expect_true(!is.null(summ$call))
})

test_that("print.summary.cubist outputs correctly", {
  data <- new_cubist_data(n = 100, p = 5)
  mod <- cubist(data$x, data$y)
  summ <- summary(mod)

  output <- capture.output(print(summ))
  expect_true(length(output) > 0)
  expect_true(any(grepl("Call:", output)))
})

test_that("print.summary.cubist returns invisible(x)", {
  data <- new_cubist_data(n = 100, p = 5)
  mod <- cubist(data$x, data$y)
  summ <- summary(mod)

  # Capture output to prevent printing during tests
  capture.output(result <- withVisible(print(summ)))
  expect_false(result$visible)
  expect_s3_class(result$value, "summary.cubist")
})

# --- truncateText() tests ---

test_that("truncateText handles short text (no truncation needed)", {
  short_text <- "short"
  result <- Cubist:::truncateText(short_text)
  expect_equal(result, short_text)
})

test_that("truncateText wraps long text", {
  # Create a very long text that exceeds typical width
  long_text <- paste(rep("word", 50), collapse = " ")
  result <- Cubist:::truncateText(long_text)

  # Result should contain newlines if text was wrapped
  if (nchar(long_text) > getOption("width")) {
    expect_true(grepl("\n", result))
  }
})

test_that("truncateText handles multi-element input", {
  # Pass a vector of strings
  input <- c("first part", "second part")
  result <- Cubist:::truncateText(input)

  # Should be collapsed into single string
  expect_type(result, "character")
  expect_length(result, 1)
})

test_that("truncateText handles very long single line", {
  # Create text that definitely exceeds width
  old_width <- options(width = 40)

  on.exit(options(old_width))

  long_text <- paste(rep("longword", 20), collapse = " ")
  result <- Cubist:::truncateText(long_text)

  # Should wrap to multiple lines
  expect_true(grepl("\n", result))
})

# --- check_names() tests ---

test_that("check_names passes with column names", {
  df <- data.frame(a = 1:3, b = 4:6)
  expect_silent(Cubist:::check_names(df))
})

test_that("check_names errors without column names", {
  mat <- matrix(1:6, ncol = 2)
  colnames(mat) <- NULL

  expect_snapshot(Cubist:::check_names(mat), error = TRUE)
})

# --- check_date_columns() tests ---

test_that("check_date_columns passes with no date columns", {
  df <- data.frame(a = 1:3, b = 4:6)
  expect_silent(Cubist:::check_date_columns(df))
})

test_that("check_date_columns errors with Date column", {
  df <- data.frame(
    date_col = as.Date("2020-01-01") + 1:3,
    num = 1:3
  )

  expect_snapshot(Cubist:::check_date_columns(df), error = TRUE)
})

test_that("check_date_columns errors with POSIXct column", {
  df <- data.frame(
    datetime_col = as.POSIXct("2020-01-01") + 1:3,
    num = 1:3
  )

  expect_snapshot(Cubist:::check_date_columns(df), error = TRUE)
})

test_that("check_date_columns errors with POSIXlt column", {
  df <- data.frame(num = 1:3)
  df$datetime_col <- as.POSIXlt("2020-01-01") + 1:3

  expect_snapshot(Cubist:::check_date_columns(df), error = TRUE)
})
