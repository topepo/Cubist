# Cubist (development version)

* Data are now passed from R to the C code as vectors. Previously, `cubist()` and `predict()` wrote the data in the text formats of the command-line version of Cubist (`.names` and `.data` files, as strings) and the C code parsed them again. Removing that step fixes several problems:

  * Factor levels `"?"` and `"N/A"` were read as missing and not-applicable values. They are now ordinary levels.

  * Levels that differed only in whitespace (e.g., `"a b"` and `"a  b"`, or `"a"` and `" a"`) were merged. They are now kept apart.

  * Factor levels with commas, colons, and other punctuation, empty-string levels, single-level factors, and names longer than 1000 characters made the C code stop (sometimes ending the R session). They now work.

  * Column names with colons, semicolons, or vertical bars were changed (e.g., `c:d` became `c\:d`), which made `cubist()` fail. Names are now kept as is. Columns that start with `sample` are no longer renamed internally.

  * Preparing the data takes much less time and the fitted object is smaller. The object no longer has a `data` string. Instead, it has `spec`, `train`, and `format_version` elements. The `names` string is still there (in the same format) because the tidyrules package parses it, but the model does not. `exportCubistFiles()` still writes the `.names` and `.data` files.

  * Models with the same data and options are the same as before. The one exception is character predictors: their levels are now sorted (in the C locale), as `factor()` would do, instead of being kept in the order they first appear. Models fitted with Cubist 0.6.0 or earlier still work with `predict()`.

* Input checks in `cubist()`, `cubistControl()`, `predict()`, and `exportCubistFiles()` now use cli and the rlang type checkers, so errors and warnings name the function that was called and the argument that is wrong (#69). When the C code stops (e.g., when all outcome values are missing), `cubist()` and `predict()` now give an error with the reason. Before, `cubist()` failed later with an unrelated error and `predict()` returned missing values.

* In `predict()`, a factor or character value that is not in the training set is now treated as missing, with a warning. Before, those rows got a C error message and an invalid prediction.

* In `predict()`, a column of `newdata` that has only missing values is treated as missing whatever its class (e.g. `data.frame(x = NA)` makes a logical column). This keeps the 0.6.0 behavior that the dendroTools package relies on. A logical column with values is used as 0/1 when the predictor was numeric in the training set. Before, `TRUE` and `FALSE` gave wrong predictions without a warning.

* Clearer errors for infinite values in the predictors, outcome, or weights, for duplicated column names, for unsupported column types (e.g., logical), for an outcome `label` that is also a column name, for a `"case weight"` column when weights are used, and for `y` or `weights` with the wrong length.

* `cubist()` now requires at least two rows with a non-missing outcome. With only one, the C code never returned and the R session had to be killed.

* `cubistControl(seed = )` is now used. Before, the C code replaced it with a value from the clock, so models with `sample > 0` could not be reproduced.

* Fixed `escapes()`, which escaped colons, semicolons, and vertical bars twice when writing the text formats (e.g. in `exportCubistFiles()`).

* The feasible ranges for `committees` and `neighbors` have been removed:

  * `cubist()` no longer limits `committees` to 100. Any integer >= 1 is allowed.

  * `predict.cubist()` no longer limits `neighbors` to 9. Any integer from 0 to the number of training set rows is allowed. The C code now sizes its neighbor buffers from the requested number of neighbors (plus room for ties) instead of using a fixed buffer of 20. Predictions for `neighbors` from 0 to 9 are unchanged. Note that a correction is only applied when at least half of the requested neighbors are closer than the average distance between training points, so large values of `neighbors` can leave more predictions uncorrected.

  * `committees` and `neighbors` must now be single whole numbers. Non-integer values, negative `neighbors`, and missing values are errors.

  * The default tuning ranges in other packages (e.g., rules' `tunable()` method for `cubist_rules()`, `dials::neighbors()`, and caret's `"cubist"` grid) still use the old limits. Expand them manually to tune over larger values.

# Cubist 0.6.0

* A new control option, `strip_time_stamps`, is used to remove date/time and duration information for better reproducability. (#63) 

* An error is thrown if a data or data/time column is passed. 

* Switched from base R `stop()` and `warning()` to cli's `cli_abort()` and `cli_warn()` for improved error messages with better formatting and context.

* Fixed compiler warnings for unused variables in C source code.

# Cubist 0.5.1

* Big fixed related to how rule values are formatted. (#59)

# Cubist 0.5.0

* Updated C code to stop overwriting keyword symbols. 

# Cubist 0.4.4

* Fixed two issues in the vignette. 

* vignette now uses `rules::tidy()` instead of `tidyrules:: tidyRules()`

# Cubist 0.4.3

* 12.8 hours after the last submission was accepted, CRAN required another because the url `https://topepo.github.io/Cubist` forwards to `https://topepo.github.io/Cubist/` (with a slash at the end). 

# Cubist 0.4.2

* Maintenance release to fix CRAN issues related to compiler issues.

* Updated GHA and tests

# Cubist 0.4.1

* Build configuration changes for Windows

# Cubist 0.4.0

* Build configuration changes for Windows

# Cubist 0.3.0

* Build configuration changes for Windows


# Cubist 0.2.4

* Maintenance release to fix CRAN issues by adding rmarkdown to Suggests.

* Re-wrote parts of the vignettes and added another for model tuning.


# Cubist 0.2.3

* Maintenance release to fix CRAN issues for GCC 10 -fno-common flag.

* `tidy_rules()` was removed since the code is now in the tidyrules package.

* Changes to better work with tibble inputs.


# Cubist 0.2.2

* Maintenance release to fix CRAN issues for C string buffers


# Cubist 0.2.1

* Fixed a bug in the code that escapes improper characters.


# Cubist 0.2.0

* Converted documentation to `roxygen2`.

* Revised some compiler options to reduce `R CMD check` warnings.




# Cubist 0.0.19

* A bug was fixed, found by Duncan Murdoch.
* A bug in predictions when sampling was used was fixed by Laurae (a random data witch/magician) \issue{1}



# Cubist 0.0.17


* Memory allocation bugs in the R version of the C code, which lead to segmentation faults, were fixed.



# Cubist 0.0.15


* A bug was fixed where the model failed if no predictors were included in the model (thanks to Gabe Gershenfeld for the find).



# Cubist 0.0.14


* The Windows build configuration was updated.

* Some files were synced with the C50 package.



# Cubist 0.0.13


* More efficient code was borrowed from the C50 package to write the data file to a string.

* A bug related to column names found by David Clifford was fixed.


# Cubist 0.0.12


* A bug was fixed that occurred when splits were determined in models with a categorical variable involving more than two categories (eg. "if X4 in {a, b}")

* Also, a bug was found in the variable importances calculations  when a variable was used only in the conditions and not the model.

* The 'doc' directory was moved into the "vignettes" directory


# Cubist 0.0.11


* cubist uses a string buffer to capture the output files that would normally get saved to external files. The package was failing R CMD check only on windows because the output files were getting garbled. `s*printf` functions work differently on Windows. On Linux, they return the size that is needed to correctly print the string.  On Windows, if there isn't enough space, they return -1.

* Also, work-arounds were added to the C sources because of R CMD check issues. `printf()`, `assert()` and `exist()` statements were removed or redefined.


# Cubist 0.0.8

* A bug was fixed for the `coef` method.



# Cubist 0.0.6

* The GPL version was revised to be version 3 (thanks to Ross Quinlan for the note).

* The variable usage statistics were saved to a data frame in called `object$usage`. The package vignette and help files were updated accordingly.

* When using `cubist`, if `x` was a vector or a single variable, an error occurred. This was fixed.

* The vignette was also expanded to show how caret can be used to tune the model and a technical error was corrected about the boosting algorithm (thanks again to Ross Quinlan).



