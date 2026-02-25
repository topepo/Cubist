# cubist errors with non-numeric outcome

    Code
      cubist(data$x, factor(data$y > 0))
    Condition
      Error:
      ! cubist models require a numeric outcome

# cubist errors with committees outside 1-100

    Code
      cubist(data$x, data$y, committees = 0)
    Condition
      Error:
      ! number of committees must be between 1 and 100

---

    Code
      cubist(data$x, data$y, committees = 101)
    Condition
      Error:
      ! number of committees must be between 1 and 100

# cubist errors when x is not data.frame or matrix

    Code
      cubist(as.list(data$x), data$y)
    Condition
      Error:
      ! x must be a matrix or data frame

# cubist errors with non-numeric weights

    Code
      cubist(data$x, data$y, weights = rep("a", 50))
    Condition
      Error:
      ! case weights must be numeric

# cubist errors with missing column names

    Code
      cubist(x_no_names, data$y)
    Condition
      Error in `check_names()`:
      ! The data should have column names

# cubistControl errors with rules outside 1-1000000

    Code
      cubistControl(rules = 0)
    Condition
      Error:
      ! number of rules must be between 1 and 1000000

---

    Code
      cubistControl(rules = 1000001)
    Condition
      Error:
      ! number of rules must be between 1 and 1000000

# cubistControl errors with extrapolation outside 0-100

    Code
      cubistControl(extrapolation = -1)
    Condition
      Error:
      ! percent extrapolation must between 0 and 100

---

    Code
      cubistControl(extrapolation = 101)
    Condition
      Error:
      ! percent extrapolation must between 0 and 100

# cubistControl errors with sample outside 0-99.9

    Code
      cubistControl(sample = -1)
    Condition
      Error:
      ! sampling percentage must be between 0.0 and 99.9

---

    Code
      cubistControl(sample = 100)
    Condition
      Error:
      ! sampling percentage must be between 0.0 and 99.9

# check_names errors without column names

    Code
      Cubist:::check_names(mat)
    Condition
      Error in `Cubist:::check_names()`:
      ! The data should have column names

# check_date_columns errors with Date column

    Code
      Cubist:::check_date_columns(df)
    Condition
      Error:
      ! Column 'date_col' has a date/datetime class. Cubist does not support date or datetime predictors. Consider converting to numeric (e.g., days since a reference date) or extracting components (year, month, day) as separate predictors.

# check_date_columns errors with POSIXct column

    Code
      Cubist:::check_date_columns(df)
    Condition
      Error:
      ! Column 'datetime_col' has a date/datetime class. Cubist does not support date or datetime predictors. Consider converting to numeric (e.g., days since a reference date) or extracting components (year, month, day) as separate predictors.

# check_date_columns errors with POSIXlt column

    Code
      Cubist:::check_date_columns(df)
    Condition
      Error:
      ! Column 'datetime_col' has a date/datetime class. Cubist does not support date or datetime predictors. Consider converting to numeric (e.g., days since a reference date) or extracting components (year, month, day) as separate predictors.

