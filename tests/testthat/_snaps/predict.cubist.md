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

# predict.cubist errors with invalid neighbors

    Code
      predict(mod, data$x, neighbors = 51)
    Condition
      Error:
      ! 'neighbors' must be a single integer between 0 and the number of training set rows (50)

---

    Code
      predict(mod, data$x, neighbors = -1)
    Condition
      Error:
      ! 'neighbors' must be a single integer between 0 and the number of training set rows (50)

---

    Code
      predict(mod, data$x, neighbors = 2.5)
    Condition
      Error:
      ! 'neighbors' must be a single integer between 0 and the number of training set rows (50)

---

    Code
      predict(mod, data$x, neighbors = NA)
    Condition
      Error:
      ! 'neighbors' must be a single integer between 0 and the number of training set rows (50)

