
## To avoid "dotplot.cubist: no visible binding for global variable 'type'":
type <- NULL

dotplot.cubist <- function(x, data = NULL, what = "splits", committee = NULL, rule = NULL, ...)
  {

    splits <- x$splits
    if(is.null(splits)) stop("No splits were used in this model")

    if(!is.null(committee)) splits <- splits[splits$committee <= committee,]
    if(!is.null(rule)) splits <-  splits[splits$rule <= rule,]

    if(max(splits$committee) == 1)
      {
        lab <- "Rule"
        splits$label <-gsub(" ", "0", format(as.character(splits$rule), justify = "right"))
      } else {
        splits$label <- paste(gsub(" ", "0", format(as.character(splits$committe), justify = "right")),
                              gsub(" ", "0", as.character(format(splits$rule), justify = "right")),
                              sep = "/")
        lab <- "Committe/Rule"
      }

    
    if(what == "splits")
      {
        if(all(splits$type == "type3")) stop("No splits of continuous predictors were made")
        out <- dotplot(label ~ percentile|variable, data = splits,
                       subset = type == "type2",
                       groups = dir,
                       panel = function(x, y, groups, subscripts)
                       {
                         plotTheme <- trellis.par.get()
                        
                        
                         y2 <- as.numeric(y)
                         groups <- groups[subscripts]
                         isLower <- grepl("<", groups)
                         for(i in seq(along = levels(y)))
                           {
                             panel.segments(0, i, 1, i,
                                            col =  plotTheme$reference.line$col,
                                            lty =  plotTheme$reference.line$lty, 
                                            lwd =  plotTheme$reference.line$lwd)
                           }
                         for(i in seq(along = x))
                           {
                             
                             if(isLower[i])
                               {
                                 panel.segments(0, y2[i], x[i], y2[i],
                                                col = plotTheme$superpose.line$col[1],
                                                lty = plotTheme$superpose.line$lty[1],
                                                lwd = plotTheme$superpose.line$lwd[1])
                               } else panel.segments(x[i], y2[i], 1, y2[i],
                                                     col = plotTheme$superpose.line$col[2],
                                                     lty = plotTheme$superpose.line$lty[2],
                                                     lwd = plotTheme$superpose.line$lwd[2])
                           }



                       },
                       xlim = c(-.05, 1.05),
                       xlab = "Training Data Coverage",
                       ylab = lab,
                       ...)
      }
    if(what == "coefs")
      {
        coefVals <- x$coefficients
        coefVals <- melt(coefVals, id.vars = c("committee", "rule"))
        coefVals <- coefVals[complete.cases(coefVals),]
        if(max(coefVals$committee) == 1)
          {
            lab <- "Rule"
            coefVals$label <-gsub(" ", "0", format(as.character(coefVals$rule), justify = "right"))
          } else {
            coefVals$label <- paste(gsub(" ", "0", format(as.character(coefVals$committe), justify = "right")),
                                    gsub(" ", "0", as.character(format(coefVals$rule), justify = "right")),
                                    sep = "/")
            lab <- "Committe/Rule"
          }
        out <- dotplot(label ~ value|variable, data = coefVals, ...)
      }
    out
  }
