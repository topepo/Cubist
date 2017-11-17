

#' @export
#' @keywords internal
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
