#' Convert Data to Text Format
#' 
#' This class converts different types of data to a format
#'  required for [`Cubist`](http://www.rulequest.com/cubist-unix.html).
#'  
#' @export
#' @keywords internal
#' @param object An object to translate to the format expected by
#'  the Quinlan C code. 
#' @return A text representation of the data.
QuinlanAttributes <-
  function (object, ...)
    UseMethod("QuinlanAttributes")

#' @export
QuinlanAttributes.numeric <- function(object, ...)
  "continuous."

#' @export
QuinlanAttributes.factor <-
  function(object, ...)
    paste(paste(levels(object), collapse = ","), ".", sep = "")

#' @export
QuinlanAttributes.character <-
  function(object, ...)
    paste(paste(unique(object), collapse = ","), ".", sep = "")

#' @export
QuinlanAttributes.ordered <-
  function(object, ...)
    paste("[ordered]", paste(levels(object), collapse = ","), ".", sep = "")

#' @export
QuinlanAttributes.matrix <-
  function(object, ...)
    apply(object, 2, QuinlanAttributes)

#' @export
QuinlanAttributes.data.frame <-
  function(object, ...)
    unlist(lapply(object,  QuinlanAttributes))
