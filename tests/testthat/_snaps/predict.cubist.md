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

# predict.cubist errors with invalid neighbors

    Code
      predict(mod, data$x, neighbors = 51)
    Condition
      Error in `predict()`:
      ! `neighbors` must be a whole number between 0 and 50, not the number 51.

---

    Code
      predict(mod, data$x, neighbors = -1)
    Condition
      Error in `predict()`:
      ! `neighbors` must be a whole number between 0 and 50, not the number -1.

---

    Code
      predict(mod, data$x, neighbors = 2.5)
    Condition
      Error in `predict()`:
      ! `neighbors` must be a whole number, not the number 2.5.

---

    Code
      predict(mod, data$x, neighbors = NA)
    Condition
      Error in `predict()`:
      ! `neighbors` must be a whole number, not `NA`.

