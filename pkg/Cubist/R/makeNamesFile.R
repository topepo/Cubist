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

makeNamesFile <-
function(x, y, label = "outcome", comments = TRUE)
  {
    if(comments)
      {
        call <- match.call()
        out <- paste("| Generated using ", R.version.string, "\n",
                     "| on ", format(Sys.time(), "%a %b %d %H:%M:%S %Y"), "\n",
                     "| function call: ", paste(deparse(call)),
                     sep = "")
      } else out <- ""

    if(is.numeric(y))
      {
        outcomeInfo <- ": continuous."
      } else {
        lvls <- levels(y)
        prefix <- if(is.ordered(y)) "[ordered] " else ""
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
    varData <- paste(escapes(names(varData)), ": ", varData, sep = "", collapse = "\n")
    out <- paste(out, "\n", varData, "\n", sep = "")
    out


  }

escapes <- function(x, chars = c(":", ";", "|"))
  {
    for(i in chars) x <- gsub(i, paste("\\", i, sep = ""), x, fixed = TRUE)
    x
  }

