## TODO:
## 3) R function to write R prediction function


countRules <- function(x)
  {
    x <- strsplit(x, "\n")[[1]]
    comNum <- ruleNum <- condNum <- rep(NA, length(x))
    comIdx <- rIdx <- 0
    for(i in seq(along = x))
      {
        tt <- parser(x[i])
        if(names(tt)[1] == "rules")
          {
            comIdx <- comIdx + 1
            rIdx <- 0
          }
        comNum[i] <-comIdx
        if(names(tt)[1] == "conds")
          {
            rIdx <- rIdx + 1
            cIdx <- 0
          }
        ruleNum[i] <-rIdx
        if(names(tt)[1] == "type")
          {
            cIdx <- cIdx + 1
            condNum[i] <- cIdx
          }
      }
    numCom <- sum(grepl("^rules=", x))
    rulesPerCom <- unlist(lapply(split(ruleNum, as.factor(comNum)), max))
    rulesPerCom <- rulesPerCom[rulesPerCom > 0]
    rulesPerCom

  }



getSplits <- function(x)
  {
    x <- strsplit(x, "\n")[[1]]
    comNum <- ruleNum <- condNum <- rep(NA, length(x))
    comIdx <- rIdx <- 0
    for(i in seq(along = x))
      {
        tt <- parser(x[i])
        ## Start of a new rule
        if(names(tt)[1] == "rules")
          {
            comIdx <- comIdx + 1
            rIdx <- 0
          }
        comNum[i] <-comIdx
        ## Start of a new condition
        if(names(tt)[1] == "conds")
          {
            rIdx <- rIdx + 1
            cIdx <- 0
          }
        ruleNum[i] <-rIdx
        ## Within a rule, type designates the type of conditional statement
        ## type = 2 appears to be a simple split of a continuous predictor
        if(names(tt)[1] == "type")
          {
            cIdx <- cIdx + 1
            condNum[i] <- cIdx
          }
      }
    
    numCom <- sum(grepl("^rules=", x))
    rulesPerCom <- unlist(lapply(split(ruleNum, as.factor(comNum)), max))
    rulesPerCom <- rulesPerCom[rulesPerCom > 0]
    if (! is.null(rulesPerCom) && numCom > 0)
      names(rulesPerCom) <- paste("Com", 1:numCom)

    ## In object x, what element starts a new rule
    isNewRule <- ifelse(grepl("^conds=", x), TRUE, FALSE)   
    splitVar <- rep("", length(x))
    splitVal <- rep(NA, length(x))
    splitCats <- rep("", length(x))
    splitDir <- rep("", length(x))

    ## This is a simple continuous split, such as
    ##
    ##   nox > 0.668
    ##
    ## or
    ##
    ## type="2" att="nox" cut="0.66799998" result=">"
    ##    
    isType2 <- grepl("^type=\"2\"", x)
    if(any(isType2))
      {
        splitVar[isType2] <- type2(x[isType2])$var
        splitVar[isType2] <- gsub("\"", "", splitVar[isType2])
        splitDir[isType2] <- type2(x[isType2])$rslt
        splitVal[isType2] <- type2(x[isType2])$val
      }
    ## This is a split of categorical data such as 
    ##
    ##   X4 in {c, d}
    ##
    ## or
    ##
    ## type="3" att="X4" elts="c","d"
    ##

    isType3 <- grepl("^type=\"3\"", x)
    if(any(isType3))
      {
        splitVar[isType3] <- type3(x[isType3])$var
        splitCats[isType3] <- type3(x[isType3])$val
        splitCats[isType3] <- gsub("[{}]", "", splitCats[isType3])
        splitCats[isType3] <- gsub("\"", "", splitCats[isType3])
        splitCats[isType3] <- gsub(" ", "", splitCats[isType3])
      }
    if(!any(isType2) & !any(isType3)) return(NULL)
    splitData <- data.frame(committee = comNum,
                            rule = ruleNum,
                            variable = splitVar,
                            dir = splitDir,
                            value = as.numeric(splitVal),
                            category = splitCats)
    splitData$type <- ""
    if(any(isType2)) splitData$type[isType2] <- "type2"
    if(any(isType3)) splitData$type[isType3] <- "type3"    
    splitData <- splitData[splitData$variable != "" ,]
    splitData
  }

## This function is no longer used
printCubistRules <- function(x, dig = max(3, getOption("digits") - 5))
  {
    
    comNum <- ruleNum <- condNum <- rep(NA, length(x))
    comIdx <- rIdx <- 0
    for(i in seq(along = x))
      {
        tt <- parser(x[i])
        if(names(tt)[1] == "rules")
          {
            comIdx <- comIdx + 1
            rIdx <- 0
          }
        comNum[i] <-comIdx
        if(names(tt)[1] == "conds")
          {
            rIdx <- rIdx + 1
            cIdx <- 0
          }
        ruleNum[i] <-rIdx
        if(names(tt)[1] == "type")
          {
            cIdx <- cIdx + 1
            condNum[i] <- cIdx
          }
      }
    
    numCom <- sum(grepl("^rules=", x))
    rulesPerCom <- unlist(lapply(split(ruleNum, as.factor(comNum)), max))
    rulesPerCom <- rulesPerCom[rulesPerCom > 0]
    names(rulesPerCom) <- paste("Com", 1:numCom)
    cat("Number of committees:", numCom, "\n")
    cat("Number of rules per committees:",
        paste(rulesPerCom, collapse = ", "), "\n\n")
   
    isNewRule <- ifelse(grepl("^conds=", x), TRUE, FALSE)
    isEqn <- ifelse(grepl("^coeff=", x), TRUE, FALSE) 
    
    cond <- rep("", length(x))
    isType2 <- grepl("^type=\"2\"", x)
    if(any(isType2)) cond[isType2] <- type2(x[isType2], dig = dig)$text
    isType3 <- grepl("^type=\"3\"", x)
    if(any(isType3)) cond[isType3] <- type3(x[isType3])$text

    isEqn <- grepl("^coeff=", x)
    eqtn <- rep("", length(x))
    eqtn[isEqn] <- eqn(x[isEqn], dig = dig)

    tmp <- x[isNewRule]
    tmp <- parser(tmp)
    ruleN <- rep(NA, length(x))
    ruleN[isNewRule] <- as.numeric(unlist(lapply(tmp, function(x) x["cover"])))
    
    for(i in seq(along = x))
      {
        if(isNewRule[i])
          {
            cat("Rule ", comNum[i], "/", ruleNum[i], ": (n=",
                ruleN[i], ")\n", sep = "")
            cat("  If\n")
          } else {
            if(cond[i] != "")
              {
                cat("  |", cond[i], "\n")
                if(cond[i+1] == "")
                  {
                    cat("  Then\n")
                    cat("    prediction =", eqtn[i+1], "\n\n")
                  }
              }
          }
      }
  }

type3 <- function(x)
  {
    aInd <- regexpr("att=", x)
    eInd <- regexpr("elts=", x)
    var <- substring(x, aInd + 4, eInd - 2)
    val <- substring(x, eInd + 5)
    multVals <- grepl(",", val)
    val <- gsub(",", ", ", val) 
    val <- ifelse(multVals, paste("{", val, "}", sep = ""), val)
    txt <- ifelse(multVals,  paste(var, "in", val),  paste(var, "=", val))
 
    list(var = var, val = val, text = txt)
  }

type2 <- function(x, dig = 3)
  {
    x <- gsub("\"", "", x)
    aInd <- regexpr("att=", x)
    cInd <- regexpr("cut=", x)
    rInd <- regexpr("result=", x)
    vInd <- regexpr("val=", x)

    var <- val <- rslt <- rep("", length(x))
    
    missingRule <- cInd < 1 & vInd > 0
    
    if(any(missingRule))
      {
        var[missingRule] <- substring(x[missingRule], aInd[missingRule] + 4, vInd[missingRule] - 2)
        val[missingRule] <- "NA"
        rslt[missingRule] <- "="  
      }
    if(any(!missingRule))
      {
        var[!missingRule] <- substring(x[!missingRule], aInd[!missingRule] + 4, cInd[!missingRule] - 2)        
        val[!missingRule] <- substring(x[!missingRule], cInd[!missingRule] + 4, rInd[!missingRule] - 1)
        val[!missingRule] <- format(as.numeric(val[!missingRule]), digits = dig)
        rslt[!missingRule] <- substring(x[!missingRule], rInd[!missingRule] + 7)
      }

    list(var = var, val = as.numeric(val), rslt = rslt,
         text = paste(var, rslt, val))
  }


eqn <- function(x, dig = 10, text = TRUE, varNames = NULL)
  {
    x <- gsub("\"", "", x)
    out <- vector(mode = "list", length = length(x))

    for(j in seq(along = x))
      {
        starts <- gregexpr("(coeff=)|(att=)", x[j])[[1]]
        p <- (length(starts) - 1)/2
        vars <- vector(mode = "numeric", length = p + 1)
        tmp <- vector(mode = "character", length = length(starts))
        
        for(i in seq(along = starts))
          {
            if(i < length(starts))
              {
                txt <- substring(x[j], starts[i], starts[i + 1] - 2)
                
              } else txt <- substring(x[j], starts[i])
            tmp[i] <- gsub("(coeff=)|(att=)", "", txt)
          }

        valSeq <- seq(1, length(tmp), by = 2)
        vals <- as.double(tmp[valSeq])

        nms <- tmp[-valSeq]

        if(text)
          {
            signs <- sign(vals)
            vals <- abs(vals)

            for(i in seq(along = vals))
              {
                if(i == 1)
                  {
                    txt <- ifelse(signs[1] == -1,
                                  format(-vals[1], digits = dig),
                                  format(vals[1], digits = dig))
                  } else {
                    tmp2 <- ifelse(signs[i] == -1,
                                   paste("-", format(vals[i], digits = dig)),
                                   paste("+", format(vals[i], digits = dig)))
                    txt <- paste(txt, tmp2, nms[i-1])
                  }
              }        
            out[j] <- txt
          } else {
            
            nms <- c("(Intercept)", nms)
            names(vals) <- nms
            if(!is.null(varNames))
              {

                vars2 <- varNames[!(varNames %in% nms)]
                #cat("j", j, "\tcoefs:", length(vals), "\tother:", length(vars2))
                vals2 <- rep(NA, length(vars2))
                names(vals2) <- vars2
                vals <- c(vals, vals2)               
                newNames <- c("(Intercept)", varNames)
                vals <- vals[newNames]
              }
            #cat("\tfinal:", length(vals), "\n")
            out[[j]] <- vals

          }
        
      }
    out
  }

parser <- function(x)
  {
    x <- strsplit(x, " ")
    x <- lapply(x,
                function(y)
                {
                  y <- strsplit(y, "=")
                  nms <- unlist(lapply(y, function(z) z[1]))
                  val <- unlist(lapply(y, function(z) z[2]))
                  names(val) <- nms
                  val
                })
    if(length(x) == 1) x <- x[[1]]
    x
  }


coef.cubist <- function(object, varNames = NULL, ...)
  {

    
    x <- object$model
    x <- strsplit(x, "\n")[[1]]
    comNum <- ruleNum <- condNum <- rep(NA, length(x))
    comIdx <- rIdx <- 0
    for(i in seq(along = x))
      {
        tt <- parser(x[i])
        if(names(tt)[1] == "rules")
          {
            comIdx <- comIdx + 1
            rIdx <- 0
          }
        comNum[i] <-comIdx
        if(names(tt)[1] == "conds")
          {
            rIdx <- rIdx + 1
            cIdx <- 0
          }
        ruleNum[i] <-rIdx
        if(names(tt)[1] == "type")
          {
            cIdx <- cIdx + 1
            condNum[i] <- cIdx
          }
      }
    isEqn <- ifelse(grepl("^coeff=", x), TRUE, FALSE) 

    isEqn <- grepl("^coeff=", x)
    coefs <- eqn(x[isEqn], dig = 0, text = FALSE, varNames = varNames)
    p <- length(coefs)
    dims <- unlist(lapply(coefs, length))
    
    coefs <- do.call("c", coefs)
    coms <- rep(comNum[isEqn], dims)
    rls <- rep(ruleNum[isEqn], dims)
    out <- data.frame(tmp = paste(coms, rls, sep = "."), value = coefs, var = names(coefs))
    out <- reshape(out, direction = "wide", v.names = "value", timevar = "var",
                   idvar = "tmp")
    colnames(out) <- gsub("value.", "", colnames(out), fixed = TRUE)
    tmp <- strsplit(as.character(out$tmp), ".", fixed = TRUE)
    out$committee <- unlist(lapply(tmp, function(x) x[1]))
    out$rule <- unlist(lapply(tmp, function(x) x[2]))
    out$tmp <- NULL
    out

  }




if(FALSE)
  {
    setwd("~/Downloads/Cubist")
    comMod <- read.delim("committee_example.model", stringsAsFactors = FALSE)[,1]
    printCubistRules(comMod, 1)
  }
