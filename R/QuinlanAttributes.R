
QuinlanAttributes <- function (x, ...) UseMethod("QuinlanAttributes")
QuinlanAttributes.numeric <- function(x, ...) "continuous."
QuinlanAttributes.factor <- function(x, ...) paste(paste(levels(x), collapse = ","), ".", sep = "")
QuinlanAttributes.character <- function(x, ...) paste(paste(unique(x), collapse = ","), ".", sep = "")
QuinlanAttributes.ordered <- function(x, ...) paste("[ordered]", paste(levels(x), collapse = ","), ".", sep = "")
QuinlanAttributes.matrix <- function(x, ...) apply(x, 2, QuinlanAttributes)
QuinlanAttributes.data.frame <- function(x, ...) unlist(lapply(x,  QuinlanAttributes))

