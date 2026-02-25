test_that("basic execution", {
  skip_if_not_installed("modeldata")

  suppressMessages(library(modeldata))
  set.seed(4827)
  dat_tr <- sim_regression(100)
  dat_te <- sim_regression(100)

  set.seed(4827)
  c5_xy <- cubist(dat_tr[, -1], y = dat_tr$outcome)
  expect_snapshot(c5_xy)

  pred_xy <- predict(c5_xy, dat_te[, -1])
  expect_equal(length(pred_xy), nrow(dat_tr))

  expect_snapshot(cubistControl())
})

test_that("strip_time_stamps removes timestamps from output", {
  skip_if_not_installed("modeldata")
  library(modeldata)
  set.seed(1)
  dat_tr <- sim_regression(100)

  # Default: timestamps stripped but version header kept
  mod <- cubist(dat_tr[, -1], y = dat_tr$outcome)
  expect_true(grepl("Cubist \\[Release .* GPL Edition\\]", mod$output))
  expect_false(grepl(
    "Cubist \\[Release .* GPL Edition\\]  [A-Z][a-z]{2} [A-Z][a-z]{2}",
    mod$output
  ))
  expect_false(grepl("Time: .* secs", mod$output))

  # With strip_time_stamps = FALSE: full timestamps present
  mod2 <- cubist(
    dat_tr[, -1],
    y = dat_tr$outcome,
    control = cubistControl(strip_time_stamps = FALSE)
  )
  expect_true(grepl(
    "Cubist \\[Release .* GPL Edition\\]  [A-Z][a-z]{2} [A-Z][a-z]{2}",
    mod2$output
  ))
  expect_true(grepl("Time: .* secs", mod2$output))
})

test_that("strip_time_stamps validation", {
  expect_snapshot(
    cubistControl(strip_time_stamps = "yes"),
    error = TRUE
  )
  expect_snapshot(
    cubistControl(strip_time_stamps = c(TRUE, FALSE)),
    error = TRUE
  )
  expect_snapshot(
    cubistControl(strip_time_stamps = NA),
    error = TRUE
  )
  expect_snapshot(
    cubistControl(strip_time_stamps = 1),
    error = TRUE
  )
})
