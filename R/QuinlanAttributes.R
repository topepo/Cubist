#' Convert Data to Text Format
#' 
#' This class converts different types of data to a format
#'  required for [`Cubist`](http://www.rulequest.com/cubist-unix.html).
#'  
#' @export
#' @keywords internal
#' @param x An object to translate to the format expected by
#'  the Quinlan C code. 
#' @return A text representation of the data.
QuinlanAttributes <-
  function (x, ...)
    UseMethod("QuinlanAttributes")

#' @export
QuinlanAttributes.numeric <- function(x, ...)
  "continuous."

#' @export
QuinlanAttributes.factor <-
  function(x, ...)
    paste(paste(levels(x), collapse = ","), ".", sep = "")

#' @export
QuinlanAttributes.character <-
  function(x, ...)
    paste(paste(unique(x), collapse = ","), ".", sep = "")

#' @export
QuinlanAttributes.ordered <-
  function(x, ...)
    paste("[ordered]", paste(levels(x), collapse = ","), ".", sep = "")

#' @export
QuinlanAttributes.matrix <-
  function(x, ...)
    apply(x, 2, QuinlanAttributes)

#' @export
QuinlanAttributes.data.frame <-
  function(x, ...)
    unlist(lapply(x,  QuinlanAttributes))
