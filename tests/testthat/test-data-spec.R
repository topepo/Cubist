# Tests for passing data to the C code as vectors (R/data-spec.R, src/rdata.c)

# Fit through the legacy text interface (.names/.data strings), as
# cubist.default() did up to version 0.6.0
fit_legacy <- function(
  x,
  y,
  w = NULL,
  committees = 1,
  control = cubistControl()
) {
  .C(
    "cubist",
    makeNamesFile(x, y, w = w, label = control$label, comments = FALSE),
    makeDataFile(x, y, w),
    as.logical(control$unbiased),
    "yes",
    1L,
    as.integer(committees),
    as.double(control$sample),
    as.integer(control$seed),
    as.integer(control$rules),
    as.double(control$extrapolation),
    model = character(1),
    output = character(1),
    PACKAGE = "Cubist"
  )
}

# cubist.default() rewrites the second line of the model (instance settings)
model_without_insts <- function(model) {
  strsplit(model, "\n", fixed = TRUE)[[1]][-2]
}

# ------------------------------------------------------------------------------
# Same results as the text interface

test_that("vector and text interfaces build identical models", {
  ctrl <- cubistControl(seed = 31)
  expect_same_fit <- function(x, y, w = NULL, committees = 1) {
    old <- fit_legacy(x, y, w = w, committees = committees, control = ctrl)
    new <- cubist(x, y, committees = committees, control = ctrl, weights = w)
    expect_identical(
      model_without_insts(new$model),
      model_without_insts(old$model)
    )
    expect_identical(new$output, strip_time_stamps(old$output))
  }

  # numeric, factor, ordered, and character predictors with missing values
  dat <- new_mixed_data(n = 150)
  dat$num[c(4, 20)] <- NA
  dat$fac[7] <- NA
  set.seed(4127)
  y <- dat$num * 2 + as.integer(dat$fac) + rnorm(150, sd = 0.2)
  y[is.na(y)] <- 0
  expect_same_fit(dat, y)
  expect_same_fit(dat, y, committees = 4)

  # case weights. The text interface does not always give the same model
  # for mixed-type data with case weights (repeated identical calls can
  # differ in the last digits), so weights are compared on numeric data.
  num <- new_cubist_data(n = 150, p = 4)
  w <- runif(150, 0.5, 2)
  expect_same_fit(num$x, num$y, w = w)
  expect_same_fit(num$x, num$y, w = w, committees = 4)
})

test_that("vector and text interfaces give identical sampled models", {
  dat <- new_cubist_data(n = 200, p = 4)
  ctrl <- cubistControl(sample = 60, seed = 77)

  old <- fit_legacy(dat$x, dat$y, control = ctrl)
  new <- cubist(dat$x, dat$y, control = ctrl)

  expect_identical(
    model_without_insts(new$model),
    model_without_insts(old$model)
  )
  expect_identical(new$output, strip_time_stamps(old$output))
  # the hold-out cases are evaluated
  expect_match(new$output, "Evaluation on test data", fixed = TRUE)
})

test_that("the seed makes sampled models reproducible", {
  dat <- new_cubist_data(n = 200, p = 4)
  ctrl <- cubistControl(sample = 60, seed = 77)

  mod_1 <- cubist(dat$x, dat$y, control = ctrl)
  mod_2 <- cubist(dat$x, dat$y, control = ctrl)
  expect_identical(mod_1$model, mod_2$model)
  expect_match(mod_1$model, "init=\"77\"", fixed = TRUE)
})

test_that("predictions from a sampled model use all new cases", {
  dat <- new_cubist_data(n = 200, p = 4)
  mod <- cubist(dat$x, dat$y, control = cubistControl(sample = 60, seed = 77))

  preds <- predict(mod, dat$x)
  expect_length(preds, 200)
  expect_false(anyNA(preds))
})

# ------------------------------------------------------------------------------
# Fitted object

test_that("fitted models store the data as vectors", {
  dat <- new_mixed_data(n = 100)
  set.seed(9)
  y <- dat$num + rnorm(100)
  mod <- cubist(dat, y)

  expect_identical(mod$format_version, 2L)
  expect_type(mod[["names"]], "character")
  expect_null(mod[["data"]])
  expect_identical(mod$spec$names, names(dat))
  expect_identical(mod$spec$discrete, c(FALSE, TRUE, TRUE, TRUE))
  expect_identical(mod$spec$ordered, c(FALSE, FALSE, TRUE, FALSE))
  expect_identical(mod$spec$levels[[4]], c("a", "b"))
  expect_type(mod$train$x[[1]], "double")
  expect_type(mod$train$x[[2]], "integer")
  expect_identical(mod$train$y, y)
  expect_null(mod$train$w)
})

# Other packages parse the `names` string (e.g. tidyrules:::varSpec()), so it
# must keep the 0.6.0 layout, with factor levels that are not escaped
test_that("the names string has the same format as in Cubist 0.6.0", {
  set.seed(12)
  x <- data.frame(
    num = rnorm(80),
    travel = factor(sample(c("Non-Travel", "Travel_Rarely"), 80, TRUE)),
    sat = factor(
      sample(c("Low", "Very_High"), 80, TRUE),
      levels = c("Low", "Very_High"),
      ordered = TRUE
    ),
    stage_2 = rnorm(80)
  )
  y <- x$num + rnorm(80)
  w <- runif(80, 0.5, 2)

  mod <- cubist(x, y)
  expect_match(mod$names, "^\\| Generated using R")
  expect_match(mod$names, "\noutcome: continuous.\n", fixed = TRUE)
  expect_match(mod$names, "\ntravel: Non-Travel,Travel_Rarely.\n", fixed = TRUE)

  # The header comments have a time stamp, so compare the rest with the text
  # that makeNamesFile() (the 0.6.0 code) writes
  new_text <- cubist_names_string(
    mod$spec,
    comments = FALSE,
    escape_values = FALSE
  )
  expect_identical(new_text, makeNamesFile(x, y, comments = FALSE))

  mod_wts <- cubist(x, y, weights = w)
  new_text_wts <- cubist_names_string(
    mod_wts$spec,
    comments = FALSE,
    escape_values = FALSE
  )
  expect_identical(new_text_wts, makeNamesFile(x, y, w = w, comments = FALSE))
})

test_that("character levels are sorted independently of the locale", {
  x <- data.frame(chr = c("b", "B", "a", NA, "A", "b"), num = 1:6)
  spec <- cubist_data_spec(x)
  expect_identical(spec$levels[[1]], c("A", "B", "a", "b"))
  cols <- cubist_data_columns(x, spec)
  expect_identical(cols[[1]], c(4L, 2L, 3L, NA, 1L, 4L))
})

test_that("models from Cubist 0.6.0 still predict", {
  legacy <- readRDS(test_path("test_data", "legacy_fit_v060.rds"))
  expect_null(legacy$fit$format_version)

  expect_identical(predict(legacy$fit, legacy$newdata), legacy$pred$p0)
  expect_identical(
    predict(legacy$fit, legacy$newdata, neighbors = 3),
    legacy$pred$p3
  )
})

# ------------------------------------------------------------------------------
# Names and values that the text formats could not represent

test_that("column names with special characters are kept as is", {
  set.seed(2290)
  n <- 120
  x <- data.frame(matrix(rnorm(n * 6), n, 6))
  names(x) <- c("a, b", "c:d", "e.f", "g|h", "sample_x", "attributes excluded")
  y <- ifelse(x[[1]] > 0, 3 * x[[1]], -x[[2]]) + rnorm(n, sd = 0.1)

  mod <- cubist(x, y)

  expect_true(all(as.character(mod$splits$variable) %in% names(x)))
  expect_true(all(names(x) %in% names(mod$coefficients)))
  expect_true(all(names(x) %in% mod$usage$Variable))
  expect_no_match(mod$model, "__Sample", fixed = TRUE)
  expect_length(predict(mod, x), n)
})

test_that("very long names and levels are allowed", {
  set.seed(52)
  long_name <- strrep("v", 1500)
  long_level <- strrep("L", 1500)
  x <- data.frame(
    num = rnorm(80),
    fac = factor(sample(c(long_level, "short"), 80, TRUE))
  )
  names(x)[1] <- long_name
  y <- x[[1]] + as.integer(x$fac) + rnorm(80, sd = 0.1)

  mod <- cubist(x, y)
  expect_true(long_name %in% mod$vars$all)
  expect_length(predict(mod, x), 80)
})

test_that("factor levels with special characters work", {
  set.seed(2290)
  f <- sample(c("a,b", "c:d", "e;f", "g|h", "i.j", "k\\l"), 150, TRUE)
  x <- data.frame(f = f, num = rnorm(150))
  y <- x$num + 3 * as.integer(factor(f)) + rnorm(150, sd = 0.1)

  mod <- cubist(x, y)
  expect_identical(mod$spec$levels[[1]], sort(unique(f), method = "radix"))
  preds <- predict(mod, x)
  expect_length(preds, 150)
  expect_false(anyNA(preds))
})

test_that("'?' and 'N/A' are ordinary factor levels", {
  set.seed(2290)
  f <- sample(c("?", "N/A", "ok"), 150, TRUE)
  x <- data.frame(f = factor(f), num = rnorm(150))
  y <- x$num + 4 * as.integer(x$f) + rnorm(150, sd = 0.1)

  mod <- cubist(x, y)
  # Each level has its own effect, so the fit is close and every level
  # gets a distinct prediction
  preds <- predict(mod, x)
  expect_lt(mean(abs(preds - y)), 0.5)
  level_means <- tapply(preds - x$num, x$f, mean)
  expect_length(unique(round(level_means)), 3)
})

test_that("levels that differ only in whitespace stay distinct", {
  set.seed(2290)
  f <- sample(c("a", " a", "a  b", "a b", ""), 200, TRUE)
  x <- data.frame(f = factor(f), num = rnorm(200))
  y <- x$num + 4 * as.integer(x$f) + rnorm(200, sd = 0.1)

  mod <- cubist(x, y)
  expect_length(mod$spec$levels[[1]], 5)
  preds <- predict(mod, x)
  expect_lt(mean(abs(preds - y)), 0.5)
})

test_that("single-level factors are allowed", {
  dat <- new_cubist_data(n = 60, p = 2)
  dat$x$f <- factor("only")

  mod <- cubist(dat$x, dat$y)
  expect_length(predict(mod, dat$x), 60)
})

test_that("a predictor may be named 'case weight' when no weights are used", {
  dat <- new_cubist_data(n = 60, p = 2)
  names(dat$x)[1] <- "case weight"

  mod <- cubist(dat$x, dat$y)
  expect_false(mod$caseWeights)
  expect_true("case weight" %in% names(mod$coefficients))
})

# ------------------------------------------------------------------------------
# New data

test_that("unseen levels in new data are treated as missing with a warning", {
  set.seed(731)
  x <- data.frame(
    f = sample(c("a", "b", "c"), 120, TRUE),
    g = factor(sample(c("u", "v"), 120, TRUE)),
    num = rnorm(120)
  )
  y <- x$num + 3 * as.integer(factor(x$f)) + rnorm(120, sd = 0.1)
  mod <- cubist(x, y)

  new_x <- data.frame(
    f = c("a", "zz", "c", "yy", "zz"),
    g = c("u", "v", "w", "u", "v"),
    num = x$num[1:5]
  )
  expect_snapshot(preds <- predict(mod, new_x))

  as_missing <- new_x
  as_missing$f[c(2, 4, 5)] <- NA
  as_missing$g[3] <- NA
  expect_identical(preds, predict(mod, as_missing))
})

# Packages such as dendroTools predict on `data.frame(x = NA)`, which makes a
# logical column; Cubist 0.6.0 treated it as missing
test_that("all-missing columns in new data are treated as missing", {
  dat <- new_cubist_data(n = 60, p = 2)
  mod <- cubist(dat$x, dat$y)

  as_numeric <- dat$x[1:5, ]
  as_numeric$x1 <- NA_real_
  expected <- predict(mod, as_numeric)

  as_logical <- as_numeric
  as_logical$x1 <- NA
  expect_identical(predict(mod, as_logical), expected)

  as_character <- as_numeric
  as_character$x1 <- NA_character_
  expect_identical(predict(mod, as_character), expected)
})

test_that("logical columns in new data are used as 0/1", {
  dat <- new_cubist_data(n = 60, p = 2)
  mod <- cubist(dat$x, dat$y)

  as_logical <- dat$x[1:4, ]
  as_logical$x1 <- c(TRUE, FALSE, NA, TRUE)
  as_numeric <- as_logical
  as_numeric$x1 <- c(1, 0, NA, 1)
  expect_identical(predict(mod, as_logical), predict(mod, as_numeric))
})

test_that("new data must keep numeric columns numeric", {
  dat <- new_cubist_data(n = 60, p = 2)
  mod <- cubist(dat$x, dat$y)
  bad <- dat$x
  bad$x1 <- as.character(bad$x1)

  expect_snapshot(predict(mod, bad), error = TRUE)
})

# ------------------------------------------------------------------------------
# Input checks

test_that("input errors", {
  dat <- new_cubist_data(n = 30, p = 2)

  clash <- dat$x
  names(clash)[1] <- "outcome"
  expect_snapshot(cubist(clash, dat$y), error = TRUE)
  expect_s3_class(
    cubist(clash, dat$y, control = cubistControl(label = "target")),
    "cubist"
  )

  weight_name <- dat$x
  names(weight_name)[1] <- "case weight"
  expect_snapshot(
    cubist(weight_name, dat$y, weights = rep(1, 30)),
    error = TRUE
  )

  dup <- dat$x
  names(dup) <- c("a", "a")
  expect_snapshot(cubist(dup, dat$y), error = TRUE)

  lgl <- dat$x
  lgl$flag <- rep(c(TRUE, FALSE), 15)
  expect_snapshot(cubist(lgl, dat$y), error = TRUE)

  inf_x <- dat$x
  inf_x$x2[3] <- Inf
  expect_snapshot(cubist(inf_x, dat$y), error = TRUE)
  expect_snapshot(cubist(dat$x, replace(dat$y, 1, -Inf)), error = TRUE)
  expect_snapshot(
    cubist(dat$x, dat$y, weights = replace(rep(1, 30), 2, Inf)),
    error = TRUE
  )

  expect_snapshot(cubist(dat$x, dat$y[-1]), error = TRUE)
  expect_snapshot(cubist(dat$x, dat$y, weights = rep(1, 29)), error = TRUE)
})

test_that("infinite values in new data are an error", {
  dat <- new_cubist_data(n = 60, p = 2)
  mod <- cubist(dat$x, dat$y)
  bad <- dat$x
  bad$x2[5] <- Inf

  expect_snapshot(predict(mod, bad), error = TRUE)
})

# ------------------------------------------------------------------------------
# Export

test_that("exported files match the text interface", {
  dat <- new_mixed_data(n = 100)
  dat$num[3] <- NA
  set.seed(15)
  y <- rnorm(100)
  w <- runif(100)
  mod <- cubist(dat, y, weights = w)

  path <- withr::local_tempdir()
  exportCubistFiles(mod, path = path, prefix = "m")

  read_file <- function(f) readChar(f, file.size(f), useBytes = TRUE)
  names_file <- read_file(file.path(path, "m.names"))
  legacy_names <- makeNamesFile(dat, y, w = w, comments = TRUE)
  # drop the comment lines, which contain a time stamp
  strip_comments <- function(x) sub("^(\\|[^\n]*\n)+", "", x)
  expect_identical(strip_comments(names_file), strip_comments(legacy_names))

  data_file <- read_file(file.path(path, "m.data"))
  expect_identical(data_file, makeDataFile(dat, y, w))
})

test_that("exported names escape factor levels", {
  x <- data.frame(f = factor(c("a,b", "c:d", "a,b", "c:d")), num = 1:4)
  spec <- cubist_data_spec(x)
  expect_snapshot(cat(cubist_names_string(spec, comments = FALSE)))
})
