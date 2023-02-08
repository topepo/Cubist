test_that("basic execution", {
  skip_if_not_installed("modeldata")

  library(modeldata)
  set.seed(1)
  dat_tr <- sim_regression(100)
  dat_te <- sim_regression(100)

  set.seed(1)
  c5_xy <- cubist(dat_tr[, -1], y = dat_tr$outcome)
  expect_snapshot(print(c5_xy))

  pred_xy <- predict(c5_xy, dat_te[, -1])
  expect_equal(length(pred_xy), nrow(dat_tr))

  expect_snapshot(cubistControl())

})
