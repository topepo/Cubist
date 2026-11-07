# unseen levels in new data are treated as missing with a warning

    Code
      preds <- predict(mod, new_x)
    Condition
      Warning:
      `newdata` has values that were not in the training set; they are treated as missing:
      * f: "zz" and "yy"
      * g: "w"

# new data must keep numeric columns numeric

    Code
      predict(mod, bad)
    Condition
      Error in `predict()`:
      ! Column x1 was numeric in the training set but is <character> in the new data.

# input errors

    Code
      cubist(clash, dat$y)
    Condition
      Error in `cubist()`:
      ! The outcome label "outcome" is also a column name in `x`.
      i Use `cubistControl(label = )` to choose a different label.

---

    Code
      cubist(weight_name, dat$y, weights = rep(1, 30))
    Condition
      Error in `cubist()`:
      ! "case weight" is reserved for the case weights and cannot be a column name in `x` when `weights` are used.

---

    Code
      cubist(dup, dat$y)
    Condition
      Error in `cubist()`:
      ! Column names of `x` must be unique.
      x Duplicated: a.

---

    Code
      cubist(lgl, dat$y)
    Condition
      Error in `cubist()`:
      ! Predictors must be numeric, factor, or character.
      x Unsupported column: flag (logical).

---

    Code
      cubist(inf_x, dat$y)
    Condition
      Error in `cubist()`:
      ! Column x2 has infinite values.
      i Use `NA` for missing values instead.

---

    Code
      cubist(dat$x, replace(dat$y, 1, -Inf))
    Condition
      Error in `cubist()`:
      ! `y` has infinite values.
      i Use `NA` for missing values instead.

---

    Code
      cubist(dat$x, dat$y, weights = replace(rep(1, 30), 2, Inf))
    Condition
      Error in `cubist()`:
      ! `weights` has infinite values.
      i Use `NA` for missing values instead.

---

    Code
      cubist(dat$x, dat$y[-1])
    Condition
      Error in `cubist()`:
      ! `y` has 29 values but `x` has 30 rows.

---

    Code
      cubist(dat$x, dat$y, weights = rep(1, 29))
    Condition
      Error in `cubist()`:
      ! `weights` has 29 values but `x` has 30 rows.

# infinite values in new data are an error

    Code
      predict(mod, bad)
    Condition
      Error in `predict()`:
      ! Column x2 has infinite values.
      i Use `NA` for missing values instead.

# errors from the C code are reported

    Code
      call_cubist(.Call("C_cubist_fit", spec, cols, rep(NA_real_, 30), NULL, FALSE,
      1L, 0, 1L, 100L, 1))
    Condition
      Error:
      ! The Cubist C code stopped with an error.
      x Ignoring cases with unknown or N/A target value
      x No cases with known target values

---

    Code
      call_cubist(.Call("C_cubist_predict", bad_spec, "", list(), 1, NULL, list()))
    Condition
      Error:
      ! The data passed to the C code are not valid.
      i This is an internal error that was detected in the Cubist package.
        Please report it at <https://github.com/topepo/Cubist/issues> with a reprex (<https://tidyverse.org/help/>) and the full backtrace.
      Caused by error:
      ! attribute spec has no element 'discrete'

# exported names escape factor levels

    Code
      cat(cubist_names_string(spec, comments = FALSE))
    Output
      
      outcome.
      
      outcome: continuous.
      f: a\,b,c\:d.
      num: continuous.

