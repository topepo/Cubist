# predict.cubist errors when newdata is NULL

    Code
      predict(mod, newdata = NULL)
    Condition
      Error in `predict()`:
      ! `newdata` must be a data frame, not `NULL`.

# predict.cubist errors with multiple neighbors values

    Code
      predict(mod, data$x, neighbors = c(1, 2))
    Condition
      Error in `predict()`:
      ! `neighbors` must be a whole number, not a double vector.

# predict.cubist errors when neighbors > 9

    Code
      predict(mod, data$x, neighbors = 10)
    Condition
      Error in `predict()`:
      ! `neighbors` must be a whole number between 0 and 9, not the number 10.

