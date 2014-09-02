makeDataFile <-
function(x, y)
  {
    if(!is.data.frame(x)) x <- as.data.frame(x)
    if(is.null(y)) y <- rep(NA_real_, nrow(x))
    x <- cbind(y, x)
    ## Determine the locations of missing values
    naIndex <- lapply(x, function(x) which(is.na(x)))
    anyNA <- any(unlist(lapply(naIndex, length)) > 0)
    x <- as.matrix(format(x, digits = 15, scientific = FALSE))
    ## remove leading white space
    x <- gsub("^[[:blank:]]*", "", x)
    ## reset missing values
    if(anyNA)
      {
        for(i in seq(along = naIndex)) if(length(naIndex[[i]]) > 0) x[naIndex[[i]],i] <- "?"
      }
    ## This line suggested by Barry Rowlingson on 04/21/12
    paste(apply(x, 1, paste, collapse=","), collapse = "\n")
  }

