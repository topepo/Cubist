# Tests for R/exportCubistFiles.R

test_that("exportCubistFiles creates .model, .names, .data files", {
  data <- new_cubist_data(n = 50, p = 3)
  mod <- cubist(data$x, data$y)

  withr::local_tempdir(pattern = "cubist_test")
  tmpdir <- getwd()

  prefix <- "test_model"
  exportCubistFiles(mod, path = tmpdir, prefix = prefix)

  expect_true(file.exists(file.path(tmpdir, paste0(prefix, ".model"))))
  expect_true(file.exists(file.path(tmpdir, paste0(prefix, ".names"))))
  expect_true(file.exists(file.path(tmpdir, paste0(prefix, ".data"))))
})

test_that("exportCubistFiles creates default prefix with timestamp", {
  data <- new_cubist_data(n = 50, p = 3)
  mod <- cubist(data$x, data$y)

  tmpdir <- withr::local_tempdir(pattern = "cubist_test")

  exportCubistFiles(mod, path = tmpdir, prefix = NULL)

  # Should create files with pattern "model{timestamp}.{ext}"
  model_files <- list.files(tmpdir, pattern = "\\.model$")
  expect_length(model_files, 1)
  expect_true(grepl("^model", model_files[1]))
})

test_that("exportCubistFiles modifies model for neighbors", {
  data <- new_cubist_data(n = 50, p = 3)
  mod <- cubist(data$x, data$y)

  tmpdir <- withr::local_tempdir(pattern = "cubist_test")
  prefix <- "neighbors_test"

  exportCubistFiles(mod, neighbors = 5, path = tmpdir, prefix = prefix)

  # Read the model file and check for neighbors setting
  model_content <- readLines(file.path(tmpdir, paste0(prefix, ".model")))
  model_text <- paste(model_content, collapse = "\n")

  expect_true(grepl("nn=\"5\"", model_text))
})

test_that("exportCubistFiles works with neighbors = 0", {
  data <- new_cubist_data(n = 50, p = 3)
  mod <- cubist(data$x, data$y)

  tmpdir <- withr::local_tempdir(pattern = "cubist_test")
  prefix <- "no_neighbors"

  exportCubistFiles(mod, neighbors = 0, path = tmpdir, prefix = prefix)

  expect_true(file.exists(file.path(tmpdir, paste0(prefix, ".model"))))
})

test_that("exportCubistFiles creates valid model file", {
  data <- new_cubist_data(n = 50, p = 3)
  mod <- cubist(data$x, data$y)

  tmpdir <- withr::local_tempdir(pattern = "cubist_test")
  prefix <- "valid_model"

  exportCubistFiles(mod, path = tmpdir, prefix = prefix)

  model_content <- readLines(file.path(tmpdir, paste0(prefix, ".model")))
  expect_true(length(model_content) > 0)
})

test_that("exportCubistFiles creates valid names file", {
  data <- new_cubist_data(n = 50, p = 3)
  mod <- cubist(data$x, data$y)

  tmpdir <- withr::local_tempdir(pattern = "cubist_test")
  prefix <- "valid_names"

  exportCubistFiles(mod, path = tmpdir, prefix = prefix)

  names_content <- readLines(file.path(tmpdir, paste0(prefix, ".names")))
  names_text <- paste(names_content, collapse = "\n")

  expect_true(grepl("outcome", names_text))
  expect_true(grepl("continuous", names_text))
})

test_that("exportCubistFiles creates valid data file", {
  data <- new_cubist_data(n = 50, p = 3)
  mod <- cubist(data$x, data$y)

  tmpdir <- withr::local_tempdir(pattern = "cubist_test")
  prefix <- "valid_data"

  exportCubistFiles(mod, path = tmpdir, prefix = prefix)

  # warn = FALSE suppresses "incomplete final line" warning
  data_content <- readLines(
    file.path(tmpdir, paste0(prefix, ".data")),
    warn = FALSE
  )

  # Should have 50 lines (one per observation)
  expect_equal(length(data_content), 50)
})

test_that("exportCubistFiles works with case weights", {
  data <- new_cubist_data(n = 50, p = 3)
  weights <- runif(50, 0.5, 2)
  mod <- cubist(data$x, data$y, weights = weights)

  tmpdir <- withr::local_tempdir(pattern = "cubist_test")
  prefix <- "weights_model"

  exportCubistFiles(mod, path = tmpdir, prefix = prefix)

  expect_true(file.exists(file.path(tmpdir, paste0(prefix, ".model"))))
  expect_true(file.exists(file.path(tmpdir, paste0(prefix, ".names"))))
  expect_true(file.exists(file.path(tmpdir, paste0(prefix, ".data"))))
})

test_that("exportCubistFiles works with multiple committees", {
  data <- new_cubist_data(n = 50, p = 3)
  mod <- cubist(data$x, data$y, committees = 3)

  tmpdir <- withr::local_tempdir(pattern = "cubist_test")
  prefix <- "multi_committee"

  exportCubistFiles(mod, path = tmpdir, prefix = prefix)

  expect_true(file.exists(file.path(tmpdir, paste0(prefix, ".model"))))
})

test_that("exportCubistFiles data file has correct number of columns", {
  data <- new_cubist_data(n = 50, p = 5)
  mod <- cubist(data$x, data$y)

  tmpdir <- withr::local_tempdir(pattern = "cubist_test")
  prefix <- "columns_test"

  exportCubistFiles(mod, path = tmpdir, prefix = prefix)

  # warn = FALSE suppresses "incomplete final line" warning
  data_content <- readLines(
    file.path(tmpdir, paste0(prefix, ".data")),
    warn = FALSE
  )
  first_line <- data_content[1]
  n_cols <- length(strsplit(first_line, ",")[[1]])

  # Should have p + 1 columns (y + predictors)
  expect_equal(n_cols, 6)
})

test_that("exportCubistFiles creates readable model file", {
  skip_if_not_installed("mlbench")

  library(mlbench)
  data(BostonHousing)

  mod <- cubist(x = BostonHousing[, -14], y = BostonHousing$medv)

  tmpdir <- withr::local_tempdir(pattern = "cubist_test")
  prefix <- "boston_model"

  exportCubistFiles(mod, path = tmpdir, prefix = prefix)

  model_content <- readLines(file.path(tmpdir, paste0(prefix, ".model")))
  model_text <- paste(model_content, collapse = "\n")

  # Model should contain rule information
  expect_true(grepl("rules", model_text))
})

test_that("exportCubistFiles with neighbors = 9", {
  data <- new_cubist_data(n = 50, p = 3)
  mod <- cubist(data$x, data$y)

  tmpdir <- withr::local_tempdir(pattern = "cubist_test")
  prefix <- "neighbors9_test"

  exportCubistFiles(mod, neighbors = 9, path = tmpdir, prefix = prefix)

  model_content <- readLines(file.path(tmpdir, paste0(prefix, ".model")))
  model_text <- paste(model_content, collapse = "\n")

  expect_true(grepl("nn=\"9\"", model_text))
})
