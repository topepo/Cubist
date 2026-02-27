# cubist errors with non-numeric outcome

    Code
      cubist(data$x, factor(data$y > 0))
    Condition
      Error in `cubist()`:
      ! `y` must be a numeric vector, not a <factor> object.

# cubist errors with committees outside 1-100

    Code
      cubist(data$x, data$y, committees = 0)
    Condition
      Error in `cubist()`:
      ! `committees` must be a whole number between 1 and 100, not the number 0.

---

    Code
      cubist(data$x, data$y, committees = 101)
    Condition
      Error in `cubist()`:
      ! `committees` must be a whole number between 1 and 100, not the number 101.

# cubist errors when x is not data.frame or matrix

    Code
      cubist(as.list(data$x), data$y)
    Condition
      Error in `cubist()`:
      ! `x` must be a data frame or a matrix, not a list.

# cubist errors with non-numeric weights

    Code
      cubist(data$x, data$y, weights = rep("a", 50))
    Condition
      Error in `cubist()`:
      ! `weights` must be a numeric vector or `NULL`, not a character vector.

# cubist errors with missing column names

    Code
      cubist(x_no_names, data$y)
    Condition
      Error in `cubist()`:
      ! `x` must have column names.

# cubistControl errors with rules outside 1-1000000

    Code
      cubistControl(rules = 0)
    Condition
      Error in `cubistControl()`:
      ! `rules` must be a whole number between 1 and 1e+06 or `NA`, not the number 0.

---

    Code
      cubistControl(rules = 1000001)
    Condition
      Error in `cubistControl()`:
      ! `rules` must be a whole number between 1 and 1e+06 or `NA`, not the number 1000001.

# cubistControl errors with extrapolation outside 0-100

    Code
      cubistControl(extrapolation = -1)
    Condition
      Error in `cubistControl()`:
      ! `extrapolation` must be a number between 0 and 100, not the number -1.

---

    Code
      cubistControl(extrapolation = 101)
    Condition
      Error in `cubistControl()`:
      ! `extrapolation` must be a number between 0 and 100, not the number 101.

# cubistControl errors with sample outside 0-99.9

    Code
      cubistControl(sample = -1)
    Condition
      Error in `cubistControl()`:
      ! `sample` must be a number between 0 and 99.9, not the number -1.

---

    Code
      cubistControl(sample = 100)
    Condition
      Error in `cubistControl()`:
      ! `sample` must be a number between 0 and 99.9, not the number 100.

# check_names errors without column names

    Code
      Cubist:::check_names(mat)
    Condition
      Error:
      ! `mat` must have column names.

# check_date_columns errors with Date column

    Code
      Cubist:::check_date_columns(df)
    Condition
      Error:
      ! Column date_col has a date/datetime class.
      x Cubist does not support date or datetime predictors.
      i Consider converting to numeric (e.g., days since a reference date) or extracting components (year, month, day) as separate predictors.

# check_date_columns errors with POSIXct column

    Code
      Cubist:::check_date_columns(df)
    Condition
      Error:
      ! Column datetime_col has a date/datetime class.
      x Cubist does not support date or datetime predictors.
      i Consider converting to numeric (e.g., days since a reference date) or extracting components (year, month, day) as separate predictors.

# check_date_columns errors with POSIXlt column

    Code
      Cubist:::check_date_columns(df)
    Condition
      Error:
      ! Column datetime_col has a date/datetime class.
      x Cubist does not support date or datetime predictors.
      i Consider converting to numeric (e.g., days since a reference date) or extracting components (year, month, day) as separate predictors.

