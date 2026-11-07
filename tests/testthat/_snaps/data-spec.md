# unseen levels in new data are treated as missing with a warning

    Code
      preds <- predict(mod, new_x)
    Condition
      Warning:
      `newdata` has values that were not in the training set; they are treated as missing:
        'f': 'zz', 'yy'
        'g': 'w'

# new data must keep numeric columns numeric

    Code
      predict(mod, bad)
    Condition
      Error:
      ! Column 'x1' was numeric in the training set but is character in the new data

# input errors

    Code
      cubist(clash, dat$y)
    Condition
      Error:
      ! The outcome label 'outcome' is also a column name in `x`. Use `cubistControl(label = )` to choose a different label.

---

    Code
      cubist(weight_name, dat$y, weights = rep(1, 30))
    Condition
      Error:
      ! 'case weight' is reserved for the case weights and cannot be a column name in `x` when `weights` are used

---

    Code
      cubist(dup, dat$y)
    Condition
      Error:
      ! Column names of `x` must be unique. Duplicated: 'a'

---

    Code
      cubist(lgl, dat$y)
    Condition
      Error:
      ! Predictors must be numeric, factor, or character. Unsupported column(s): 'flag' (logical)

---

    Code
      cubist(inf_x, dat$y)
    Condition
      Error:
      ! Column 'x2' has infinite values; use NA for missing values instead

---

    Code
      cubist(dat$x, replace(dat$y, 1, -Inf))
    Condition
      Error:
      ! The outcome has infinite values; use NA for missing values instead

---

    Code
      cubist(dat$x, dat$y, weights = replace(rep(1, 30), 2, Inf))
    Condition
      Error:
      ! `weights` has infinite values; use NA for missing values instead

---

    Code
      cubist(dat$x, dat$y[-1])
    Condition
      Error:
      ! `y` has 29 values but `x` has 30 rows

---

    Code
      cubist(dat$x, dat$y, weights = rep(1, 29))
    Condition
      Error:
      ! `weights` has 29 values but `x` has 30 rows

# infinite values in new data are an error

    Code
      predict(mod, bad)
    Condition
      Error:
      ! Column 'x2' has infinite values; use NA for missing values instead

# exported names escape factor levels

    Code
      cat(cubist_names_string(spec, comments = FALSE))
    Output
      
      outcome.
      
      outcome: continuous.
      f: a\,b,c\:d.
      num: continuous.

