#' Convert Data to Text Format
#' 
#' This class converts different types of data to a format
#'  required for [`Cubist`](http://www.rulequest.com/cubist-unix.html)
#'  and [`C5,0`](http://www.rulequest.com/see5-unix.html).
#'  
#' @export
#' @keywords internal
#' @param object An object to translate to the format expected by
#'  the Quinlan C code. 
#' @return A textual representation of the data.
QuinlanAttributes <-
  function (object, ...)
    UseMethod("QuinlanAttributes")

#' @export
#' @rdname QuinlanAttributes
#' @method QuinlanAttributes numeric
QuinlanAttributes.numeric <- function(object, ...)
  "continuous."

#' @export
#' @rdname QuinlanAttributes
#' @method QuinlanAttributes factor
QuinlanAttributes.factor <-
  function(object, ...)
    paste(paste(levels(object), collapse = ","), ".", sep = "")

#' @export
#' @rdname QuinlanAttributes
#' @method QuinlanAttributes character
QuinlanAttributes.character <-
  function(object, ...)
    paste(paste(unique(object), collapse = ","), ".", sep = "")

#' @export
#' @rdname QuinlanAttributes
#' @method QuinlanAttributes ordered
QuinlanAttributes.ordered <-
  function(object, ...)
    paste("[ordered]", paste(levels(object), collapse = ","), ".", sep = "")

#' @export
#' @rdname QuinlanAttributes
#' @method QuinlanAttributes Date
QuinlanAttributes.Date <-
  function(object, ...)
    format(object)

#' @export
#' @rdname QuinlanAttributes
#' @method QuinlanAttributes POSIXct
QuinlanAttributes.POSIXct <-
  function(object, ...)
    format(as.Date(object))


#' @export
#' @rdname QuinlanAttributes
#' @method QuinlanAttributes matrix
QuinlanAttributes.matrix <-
  function(object, ...)
    apply(object, 2, QuinlanAttributes)

#' @export
#' @rdname QuinlanAttributes
#' @method QuinlanAttributes data.frame
QuinlanAttributes.data.frame <-
  function(object, ...) {
    if (inherits(object, "tbl_df")) {
      object <- as.data.frame(object)
    }
    unlist(lapply(object,  QuinlanAttributes))
}
