# cubist handles percent sign in variable names

    Code
      mod <- cubist(x, y)
    Condition
      Warning in `FUN()`:
      NAs introduced by coercion

# cubist errors with Date predictor

    Code
      cubist(x, y)
    Condition
      Error:
      ! Column 'date_var' has a date/datetime class. Cubist does not support date or datetime predictors. Consider converting to numeric (e.g., days since a reference date) or extracting components (year, month, day) as separate predictors.

# cubist errors with POSIXct predictor

    Code
      cubist(x, y)
    Condition
      Error:
      ! Column 'datetime_var' has a date/datetime class. Cubist does not support date or datetime predictors. Consider converting to numeric (e.g., days since a reference date) or extracting components (year, month, day) as separate predictors.

# cubist error message lists all date columns

    Code
      cubist(x, y)
    Condition
      Error:
      ! Columns 'date1', 'date2' have a date/datetime class. Cubist does not support date or datetime predictors. Consider converting to numeric (e.g., days since a reference date) or extracting components (year, month, day) as separate predictors.

