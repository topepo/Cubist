# C50 0.4.0

* Build configuration changes for Windows

# C50 0.3.0

* Build configuration changes for Windows


# C50 0.2.4

* Maintenance release to fix CRAN issues by adding rmarkdown to Suggests.

* Re-wrote parts of the vignettes and added another for model tuning.


# C50 0.2.3

* Maintenance release to fix CRAN issues for GCC 10 -fno-common flag.

* `tidy_rules()` was removed since the code is now in the tidyrules package.

* Changes to better work with tibble inputs.


# C50 0.2.2

* Maintenance release to fix CRAN issues for C string buffers


# C50 0.2.1

* Fixed a bug in the code that escapes improper characters.


# C50 0.2.0

* Converted documentation to `roxygen2`.

* Revised some compiler options to reduce `R CMD check` warnings.




# C50 0.0.19

* A bug was fixed, found by Duncan Murdoch.
* A bug in predictions when sampling was used was fixed by Laurae (a random data witch/magician) \issue{1}



# C50 0.0.17


* Memory allocation bugs in the R version of the C code, which lead to segmentation faults, were fixed.



# C50 0.0.15


* A bug was fixed where the model failed if no predictors were included in the model (thanks to Gabe Gershenfeld for the find).



# C50 0.0.14


* The Windows build configuration was updated.

* Some files were synced with the C50 package.



# C50 0.0.13


* More efficient code was borrowed from the C50 package to write the data file to a string.

* A bug related to column names found by David Clifford was fixed.


# C50 0.0.12


* A bug was fixed that occurred when splits were determined in models with a categorical variable involving more than two categories (eg. "if X4 in {a, b}")

* Also, a bug was found in the variable importances calculations  when a variable was used only in the conditions and not the model.

* The 'doc' directory was moved into the "vignettes" directory


# C50 0.0.11


* cubist uses a string buffer to capture the output files that would normally get saved to external files. The package was failing R CMD check only on windows because the output files were getting garbled. `s*printf` functions work differently on Windows. On Linux, they return the size that is needed to correctly print the string.  On Windows, if there isn't enough space, they return -1.

* Also, work-arounds were added to the C sources because of R CMD check issues. `printf()`, `assert()` and `exist()` statements were removed or redefined.


# C50 0.0.8

* A bug was fixed for the `coef` method.



# C50 0.0.6

* The GPL version was revised to be version 3 (thanks to Ross Quinlan for the note).

* The variable usage statistics were saved to a data frame in called `object$usage`. The package vignette and help files were updated accordingly.

* When using `cubist`, if `x` was a vector or a single variable, an error occurred. This was fixed.

* The vignette was also expanded to show how caret can be used to tune the model and a technical error was corrected about the boosting algorithm (thanks again to Ross Quinlan).



