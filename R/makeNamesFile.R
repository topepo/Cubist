## TODO:
##     fix call printing


## What's in a name? (http://www.rulequest.com/cubist-unix.html)
##
## Names, labels, and discrete values are represented by arbitrary
## strings of characters, with some fine print:
##
## Tabs and spaces are permitted inside a name or value, but Cubist
## collapses every sequence of these characters to a single space.
##
## Special characters (comma, colon, period, vertical bar `|') can
## appear in names and values, but must be prefixed by the escape
## character `\'. For example, the name "Filch, Grabbit, and Co."
## would be written as `Filch\, Grabbit\, and Co\.'. (However, it is
## not necessary to escape colons in times and periods in numbers.)
##
## Whitespace (blank lines, spaces, and tab characters) is ignored
## except inside a name or value and can be used to improve
## legibility. Unless it is escaped as above, the vertical bar `|'
## causes the remainder of the line to be ignored and is handy for
## including comments. When used in this way, `|' should not occur
## inside a value.
##
## The first important entry of the names file identifies the
## attribute that contains the target value -- the value to be modeled
## in terms of the other attributes -- here, fuel cost. This attribute
## must be of type continuous or an implicitly-defined attribute that
## has numeric values (see below).
##
## Following this entry, all attributes are defined in the order that
## their values will be given for each case.

#' @export
#' @keywords external
#' @rdname QuinlanAttributes
#' @param x A data frame or matrix or predictors
#' @param y A vector of outcomes
#' @param w A numeric vector of case weights or `NULL`
#' @param label A string required by the C code as the file name
#' @param comments A logical; should info about the call be
#'  printed in the string?
makeNamesFile <-
  function(x,
           y,
           w = NULL,
           label = "outcome",
           comments = TRUE) {
    if (!is.data.frame(x) || inherits(x, "tbl_df")) {
      x <- as.data.frame(x)
    }
    # See issue #5
    check_names(x)
    has_sample <- grep("^sample", colnames(x))
    if(length(has_sample))
      colnames(x) <- gsub("^sample", "__Sample", colnames(x))

    if (comments) {
      call <- match.call()
      out <- paste0(
        "| Generated using ",
        R.version.string,
        "\n",
        "| on ",
        format(Sys.time(), "%a %b %d %H:%M:%S %Y")
      )
    } else
      out <- ""

    if (is.numeric(y)) {
      outcomeInfo <- ": continuous."
    } else {
      lvls <- escapes(levels(y))
      prefix <- if (is.ordered(y))
        "[ordered] "
      else
        ""
      outcomeInfo <- paste(": ",
                           prefix,
                           paste(lvls, collapse = ","),
                           ".", sep = "")
    }

    out <- paste(out,
                 "\n", label, ".\n",
                 "\n", label, outcomeInfo,
                 sep = "")
    varData <- QuinlanAttributes(x)
    if (!is.null(w))
      varData <- c(varData, "case weight" = "continuous.")
    varData <-
      paste(escapes(names(varData)),
            ": ",
            varData,
            sep = "",
            collapse = "\n")
    out <- paste(out, "\n", varData, "\n", sep = "")
    out
  }

escapes <- function(x, chars = c(":", ";", "|")) {
  for (i in chars)
    x <- gsub(i, paste("\\", i, sep = ""), x, fixed = TRUE)
  gsub("([^[:alnum:]^[:space:]])", "\\\\\\1", x, useBytes = TRUE)
}

