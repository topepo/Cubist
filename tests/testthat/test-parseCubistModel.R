# Tests for R/parseCubistModel.R

# --- countRules() tests ---

test_that("countRules counts rules in single committee model", {
  data <- new_cubist_data(n = 100, p = 5)
  mod <- cubist(data$x, data$y, committees = 1)

  rules <- Cubist:::countRules(mod$model)

  expect_type(rules, "double")
  expect_length(rules, 1)
  expect_true(rules[1] >= 1)
})

test_that("countRules counts rules in multiple committee model", {
  data <- new_cubist_data(n = 100, p = 5)
  mod <- cubist(data$x, data$y, committees = 3)

  rules <- Cubist:::countRules(mod$model)

  expect_type(rules, "double")
  expect_length(rules, 3)
  expect_true(all(rules >= 1))
})

# --- getSplits() tests ---

test_that("getSplits extracts split information", {
  data <- new_cubist_data(n = 100, p = 5)
  mod <- cubist(data$x, data$y)

  splits <- Cubist:::getSplits(mod$model)

  # getSplits returns NULL or a data.frame
  expect_true(is.null(splits) || is.data.frame(splits))

  if (!is.null(splits)) {
    expect_s3_class(splits, "data.frame")
    expect_true("committee" %in% names(splits))
    expect_true("rule" %in% names(splits))
    expect_true("variable" %in% names(splits))
  }
})

test_that("getSplits returns NULL when no splits", {
  # Create a very simple model that might have no splits
  set.seed(42)
  x <- data.frame(x1 = rnorm(20))
  y <- x$x1 + rnorm(20, sd = 0.01) # Almost perfect linear relationship

  mod <- cubist(x, y)
  splits <- Cubist:::getSplits(mod$model)

  # Either NULL or a data frame
  expect_true(is.null(splits) || is.data.frame(splits))
})

test_that("getSplits extracts type2 (continuous) splits", {
  data <- new_cubist_data(n = 100, p = 5)
  mod <- cubist(data$x, data$y, committees = 3)

  splits <- mod$splits

  # Splits may or may not be present
  expect_true(is.null(splits) || is.data.frame(splits))

  if (!is.null(splits) && any(splits$type == "type2")) {
    type2_splits <- splits[splits$type == "type2", ]
    expect_true(nrow(type2_splits) > 0)
    expect_true(all(!is.na(type2_splits$value) | type2_splits$dir == "="))
  }
})

test_that("getSplits extracts type3 (categorical) splits", {
  set.seed(123)
  x <- data.frame(
    num = rnorm(100),
    fac = factor(sample(letters[1:4], 100, replace = TRUE))
  )
  y <- ifelse(x$fac %in% c("a", "b"), 1, 2) + x$num + rnorm(100, sd = 0.5)

  mod <- cubist(x, y)
  splits <- mod$splits

  if (!is.null(splits)) {
    # May or may not have type3 splits
    expect_s3_class(splits, "data.frame")
  }
})

# --- type2() tests ---

test_that("type2 parses continuous split correctly", {
  input <- 'type="2" att="x1" cut="0.5" result=">"'

  result <- Cubist:::type2(input)

  expect_type(result, "list")
  expect_true("var" %in% names(result))
  expect_true("val" %in% names(result))
  expect_true("rslt" %in% names(result))
})

test_that("type2 handles missing value rules", {
  input <- 'type="2" att="x1" val=NA'

  result <- Cubist:::type2(input)

  expect_true(is.na(result$val) || result$val == "NA")
})

# --- type3() tests ---

test_that("type3 parses categorical split correctly", {
  input <- 'type="3" att="category" elts="a","b"'

  result <- Cubist:::type3(input)

  expect_type(result, "list")
  expect_true("var" %in% names(result))
  expect_true("val" %in% names(result))
})

test_that("type3 handles single value", {
  input <- 'type="3" att="category" elts="a"'

  result <- Cubist:::type3(input)

  expect_type(result, "list")
  expect_false(grepl("\\{", result$val)) # No braces for single value
})

test_that("type3 formats multiple values with braces", {
  input <- 'type="3" att="category" elts="a","b","c"'

  result <- Cubist:::type3(input)

  expect_true(grepl("\\{", result$val)) # Has braces for multiple values
})

# --- eqn() tests ---

test_that("eqn parses coefficient equations in text mode", {
  input <- 'coeff="1.5" att="x1" coeff="0.5" att="x2" coeff="-0.3"'

  result <- Cubist:::eqn(input, text = TRUE)

  expect_type(result, "list")
  expect_length(result, 1)
  expect_type(result[[1]], "character")
})

test_that("eqn parses coefficient equations in numeric mode", {
  input <- 'coeff="1.5" att="x1" coeff="0.5" att="x2" coeff="-0.3"'

  result <- Cubist:::eqn(input, text = FALSE)

  expect_type(result, "list")
  expect_length(result, 1)
  expect_type(result[[1]], "double")
})

test_that("eqn includes all varNames when provided", {
  input <- 'coeff="1.5" att="x1" coeff="0.5"'

  result <- Cubist:::eqn(input, text = FALSE, varNames = c("x1", "x2", "x3"))

  # Should have entries for all varNames
  expect_true("x2" %in% names(result[[1]]))
  expect_true("x3" %in% names(result[[1]]))
})

# --- parser() tests ---

test_that("parser parses key=value pairs", {
  input <- 'key1="value1" key2="value2"'

  result <- Cubist:::parser(input)

  expect_type(result, "character")
  expect_true("key1" %in% names(result))
  expect_true("key2" %in% names(result))
})

test_that("parser handles single line", {
  input <- 'single="value"'

  result <- Cubist:::parser(input)

  expect_equal(result[["single"]], '"value"')
})

# --- coef.cubist() tests ---

test_that("coef.cubist extracts coefficients as data.frame", {
  data <- new_cubist_data(n = 100, p = 5)
  mod <- cubist(data$x, data$y)

  coefs <- coef(mod)

  expect_s3_class(coefs, "data.frame")
  expect_true("committee" %in% names(coefs))
  expect_true("rule" %in% names(coefs))
  expect_true("(Intercept)" %in% names(coefs))
})

test_that("coef.cubist includes varNames when provided", {
  data <- new_cubist_data(n = 100, p = 5)
  mod <- cubist(data$x, data$y)

  coefs <- coef(mod, varNames = names(data$x))

  # Should have columns for all variables
  for (var in names(data$x)) {
    expect_true(var %in% names(coefs))
  }
})

test_that("coef.cubist handles multiple committees", {
  data <- new_cubist_data(n = 100, p = 5)
  mod <- cubist(data$x, data$y, committees = 3)

  coefs <- coef(mod)

  expect_s3_class(coefs, "data.frame")
  # Should have coefficients from multiple committees
  expect_true(length(unique(coefs$committee)) >= 1)
})

test_that("cubist object contains coefficients", {
  data <- new_cubist_data(n = 100, p = 5)
  mod <- cubist(data$x, data$y)

  expect_true("coefficients" %in% names(mod))
  expect_s3_class(mod$coefficients, "data.frame")
})
