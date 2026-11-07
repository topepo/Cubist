# basic execution

    Code
      c5_xy
    Output
      
      Call:
      cubist.default(x = dat_tr[, -1], y = dat_tr$outcome)
      
      Number of samples: 100 
      Number of predictors: 20 
      
      Number of committees: 1 
      Number of rules: 2 
      

---

    Code
      cubistControl()
    Output
      $unbiased
      [1] FALSE
      
      $rules
      [1] 100
      
      $extrapolation
      [1] 1
      
      $sample
      [1] 0
      
      $label
      [1] "outcome"
      
      $seed
      [1] 3289
      
      $strip_time_stamps
      [1] TRUE
      

# strip_time_stamps validation

    Code
      cubistControl(strip_time_stamps = "yes")
    Condition
      Error in `cubistControl()`:
      ! `strip_time_stamps` must be `TRUE` or `FALSE`, not the string "yes".

---

    Code
      cubistControl(strip_time_stamps = c(TRUE, FALSE))
    Condition
      Error in `cubistControl()`:
      ! `strip_time_stamps` must be `TRUE` or `FALSE`, not a logical vector.

---

    Code
      cubistControl(strip_time_stamps = NA)
    Condition
      Error in `cubistControl()`:
      ! `strip_time_stamps` must be `TRUE` or `FALSE`, not `NA`.

---

    Code
      cubistControl(strip_time_stamps = 1)
    Condition
      Error in `cubistControl()`:
      ! `strip_time_stamps` must be `TRUE` or `FALSE`, not the number 1.

