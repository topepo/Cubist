# Tests for R/QuinlanAttributes.R

test_that("QuinlanAttributes.numeric returns 'continuous.'", {
  result <- QuinlanAttributes(c(1.0, 2.0, 3.0))
  expect_equal(result, "continuous.")
})

test_that("QuinlanAttributes.factor returns comma-separated levels", {
  fac <- factor(c("a", "b", "c"), levels = c("a", "b", "c"))
  result <- QuinlanAttributes(fac)
  expect_equal(result, "a,b,c.")
})

test_that("QuinlanAttributes.factor handles factor with unused levels", {
  fac <- factor(c("a", "b"), levels = c("a", "b", "c"))
  result <- QuinlanAttributes(fac)
  # Should include all levels, even unused
  expect_equal(result, "a,b,c.")
})

test_that("QuinlanAttributes.character returns comma-separated unique values", {
  chr <- c("cat", "dog", "cat", "bird")
  result <- QuinlanAttributes(chr)

  # Should contain unique values
  expect_true(grepl("cat", result))
  expect_true(grepl("dog", result))
  expect_true(grepl("bird", result))
  expect_true(grepl("\\.$", result)) # ends with period
})

test_that("QuinlanAttributes.ordered returns [ordered] prefix", {
  ord <- ordered(c("low", "med", "high"), levels = c("low", "med", "high"))
  result <- QuinlanAttributes(ord)

  expect_true(grepl("^\\[ordered\\]", result))
  expect_true(grepl("low", result))
  expect_true(grepl("med", result))
  expect_true(grepl("high", result))
})

test_that("QuinlanAttributes.Date returns formatted date", {
  dates <- as.Date(c("2020-01-01", "2020-12-31"))
  result <- QuinlanAttributes(dates)

  expect_type(result, "character")
  expect_length(result, 2)
})

test_that("QuinlanAttributes.POSIXct returns formatted as Date", {
  times <- as.POSIXct(c("2020-01-01 12:00:00", "2020-12-31 23:59:59"))
  result <- QuinlanAttributes(times)

  expect_type(result, "character")
  expect_length(result, 2)
})

test_that("QuinlanAttributes.matrix applies column-wise", {
  mat <- matrix(1:6, nrow = 2, ncol = 3)
  colnames(mat) <- c("a", "b", "c")
  result <- QuinlanAttributes(mat)

  expect_length(result, 3)
  expect_all_equal(result, "continuous.")
})

test_that("QuinlanAttributes.data.frame handles multiple column types", {
  df <- data.frame(
    num = c(1, 2, 3),
    fac = factor(c("a", "b", "c")),
    chr = c("x", "y", "z"),
    stringsAsFactors = FALSE
  )

  result <- QuinlanAttributes(df)

  expect_length(result, 3)
  expect_equal(result[["num"]], "continuous.")
  expect_equal(result[["fac"]], "a,b,c.")
})

test_that("QuinlanAttributes.data.frame handles tibbles", {
  skip_if_not_installed("dplyr")

  df <- dplyr::tibble(
    num = c(1, 2, 3),
    fac = factor(c("a", "b", "c"))
  )

  result <- QuinlanAttributes(df)

  expect_length(result, 2)
  expect_equal(result[["num"]], "continuous.")
})

test_that("QuinlanAttributes handles ordered factors in data.frame", {
  df <- data.frame(
    num = c(1, 2, 3),
    ord = ordered(c("low", "med", "high"), levels = c("low", "med", "high"))
  )

  result <- QuinlanAttributes(df)

  expect_true(grepl("\\[ordered\\]", result[["ord"]]))
})

# --- makeDataFile() tests ---

test_that("makeDataFile creates comma-separated output", {
  df <- data.frame(x1 = c(1, 2), x2 = c(3, 4))
  y <- c(5, 6)

  result <- Cubist:::makeDataFile(df, y)

  expect_type(result, "character")
  expect_true(grepl(",", result))
  expect_true(grepl("\n", result)) # Multiple lines
})

test_that("makeDataFile converts tibble", {
  skip_if_not_installed("dplyr")

  df <- dplyr::tibble(x1 = c(1, 2), x2 = c(3, 4))
  y <- c(5, 6)

  result <- Cubist:::makeDataFile(df, y)
  expect_type(result, "character")
})

test_that("makeDataFile escapes factor/character values", {
  df <- data.frame(
    chr = c("a:b", "c;d"),
    stringsAsFactors = FALSE
  )
  y <- c(1, 2)

  result <- Cubist:::makeDataFile(df, y)
  # Special characters should be escaped
  expect_type(result, "character")
})

test_that("makeDataFile replaces NULL y with NA", {
  df <- data.frame(x1 = c(1, 2), x2 = c(3, 4))

  result <- Cubist:::makeDataFile(df, y = NULL)

  expect_type(result, "character")
  expect_true(grepl("\\?", result)) # NA becomes "?"
})

test_that("makeDataFile appends weights column", {
  df <- data.frame(x1 = c(1, 2), x2 = c(3, 4))
  y <- c(5, 6)
  w <- c(1.0, 2.0)

  result <- Cubist:::makeDataFile(df, y, w = w)

  # Should have 4 columns: y, x1, x2, w
  lines <- strsplit(result, "\n")[[1]]
  expect_equal(length(strsplit(lines[1], ",")[[1]]), 4)
})

test_that("makeDataFile converts NA values to '?'", {
  df <- data.frame(x1 = c(1, NA), x2 = c(NA, 4))
  y <- c(5, 6)

  result <- Cubist:::makeDataFile(df, y)

  expect_true(grepl("\\?", result))
})

# --- makeNamesFile() tests ---

test_that("makeNamesFile creates names file string", {
  df <- data.frame(x1 = c(1, 2), x2 = c(3, 4))
  y <- c(5, 6)

  result <- Cubist:::makeNamesFile(df, y)

  expect_type(result, "character")
  expect_true(grepl("outcome", result))
  expect_true(grepl("continuous", result))
})

test_that("makeNamesFile converts tibble", {
  skip_if_not_installed("dplyr")

  df <- dplyr::tibble(x1 = c(1, 2), x2 = c(3, 4))
  y <- c(5, 6)

  result <- Cubist:::makeNamesFile(df, y)
  expect_type(result, "character")
})

test_that("makeNamesFile renames 'sample' column to '__Sample'", {
  df <- data.frame(sample = c(1, 2), other = c(3, 4))
  y <- c(5, 6)

  result <- Cubist:::makeNamesFile(df, y)

  # Underscores get escaped, so look for the escaped version
  expect_true(grepl("_Sample", result))
  expect_false(grepl("sample:", result, ignore.case = FALSE))
})

test_that("makeNamesFile includes R version in comments", {
  df <- data.frame(x1 = c(1, 2))
  y <- c(3, 4)

  result <- Cubist:::makeNamesFile(df, y, comments = TRUE)

  expect_true(grepl("Generated using R", result))
})

test_that("makeNamesFile without comments", {
  df <- data.frame(x1 = c(1, 2))
  y <- c(3, 4)

  result <- Cubist:::makeNamesFile(df, y, comments = FALSE)

  expect_false(grepl("Generated using R", result))
  # Should still have the outcome info
  expect_true(grepl("outcome", result))
})

test_that("makeNamesFile handles numeric outcome", {
  df <- data.frame(x1 = c(1, 2))
  y <- c(3, 4)

  result <- Cubist:::makeNamesFile(df, y)

  expect_true(grepl("continuous", result))
})

test_that("makeNamesFile handles factor outcome with levels", {
  df <- data.frame(x1 = c(1, 2, 3))
  y <- factor(c("a", "b", "c"))

  result <- Cubist:::makeNamesFile(df, y)

  expect_true(grepl("a,b,c", result))
})

test_that("makeNamesFile handles ordered factor outcome", {
  df <- data.frame(x1 = c(1, 2, 3))
  y <- ordered(c("low", "med", "high"), levels = c("low", "med", "high"))

  result <- Cubist:::makeNamesFile(df, y)

  expect_true(grepl("\\[ordered\\]", result))
})

test_that("makeNamesFile adds case weight entry when w provided", {
  df <- data.frame(x1 = c(1, 2))
  y <- c(3, 4)
  w <- c(1.0, 2.0)

  result <- Cubist:::makeNamesFile(df, y, w = w)

  expect_true(grepl("case weight", result))
})

test_that("makeNamesFile accepts custom label", {
  df <- data.frame(x1 = c(1, 2))
  y <- c(3, 4)

  result <- Cubist:::makeNamesFile(df, y, label = "my_target")

  expect_true(grepl("my_target", result))
})

# --- escapes() tests ---

test_that("escapes handles colons", {
  result <- Cubist:::escapes("a:b")
  expect_true(grepl("\\\\:", result))
})

test_that("escapes handles semicolons", {
  result <- Cubist:::escapes("a;b")
  expect_true(grepl("\\\\;", result))
})

test_that("escapes handles pipes", {
  result <- Cubist:::escapes("a|b")
  expect_true(grepl("\\\\\\|", result))
})

test_that("escapes preserves alphanumeric and spaces", {
  result <- Cubist:::escapes("hello world 123")
  expect_equal(result, "hello world 123")
})

test_that("escapes handles multiple special characters", {
  result <- Cubist:::escapes("a:b;c|d")
  expect_type(result, "character")
})
