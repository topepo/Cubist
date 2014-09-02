varUsage <- function(x)
  {

    x <- strsplit(x, "\n")[[1]]
    startVars <- grep("\tAttribute usage", x)
    if(length(startVars) != 1) stop("cannot find attribute usage data")
    x <- x[startVars:(length(x) - 1)]
    
    ## making some assumptions here
    x <- gsub("^\t", "", x)
    hasPct <- grep("%", x)
    if(length(hasPct) < 1) return(NULL)
    x <- x[hasPct]

    x <- as.list(x)

    getValues <- function(x)
      {

        x2 <- strsplit(x, " ")[[1]]
        hasPct <- grepl("%", x2)
        if(sum(hasPct) == 2)
          {
            x2 <- x2[grepl("%", x2)]
            x2 <- as.numeric(gsub("%", "", x2))
            return(x2)[1]
          } else {
            if(sum(hasPct) == 1)
              {
                pctInd <-  grep("%", x2)
                ## more "" in behind than in front indicate condition
                ## only
                if(sum(x2[1:pctInd] == "") < sum(x2[-(1:pctInd)] == ""))
                  {
                    x2 <- x2[grepl("%", x2)]
                    x2 <- as.numeric(gsub("%", "", x2))
                    return(c(x2, 0))

                  } else {
                    x2 <- x2[grepl("%", x2)]
                    x2 <- as.numeric(gsub("%", "", x2))
                    return(c(0, x2))
                  }
              } else return(0)
          }
      }
    getVar <- function(x)
      {
        x <- strsplit(x, " ")[[1]]
        x[length(x)]
      }
    values <- lapply(x, getValues)
    values <- do.call("rbind", values)
    values <- as.data.frame(values)
    colnames(values) <- c("Conditions", "Model")
    values$Variable <- unlist(lapply(x, getVar))
    values
    
  }
