# predict.cubist errors when newdata is NULL

    Code
      predict(mod, newdata = NULL)
    Condition
      Error:
      ! newdata must be non-null

# predict.cubist errors with multiple neighbors values

    Code
      predict(mod, data$x, neighbors = c(1, 2))
    Condition
      Error in `predict.cubist()`:
      ! only a single value of neighbors is allowed

# predict.cubist errors when neighbors > 9

    Code
      predict(mod, data$x, neighbors = 10)
    Condition
      Error in `predict.cubist()`:
      ! 'neighbors' must be less than 10

