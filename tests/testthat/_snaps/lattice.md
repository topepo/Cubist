# dotplot.cubist errors when no splits in model

    Code
      dotplot(mod_no_splits)
    Condition
      Error in `dotplot()`:
      ! No splits were used in this model.

# dotplot.cubist errors with only categorical splits

    Code
      dotplot(mod, what = "splits")
    Condition
      Error in `dotplot()`:
      ! No splits of continuous predictors were made.

