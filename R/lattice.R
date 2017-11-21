#' Visualization of Cubist Rules and Equations
#' 
#'   Lattice dotplots of the rule conditions or the linear model
#'  coefficients produced by [cubist()] objects
#' 
#'   For the splits, a panel is created for each predictor. The
#'  x-axis is the range of the predictor scaled to be between zero
#'  and one and the y-axis has a line for each rule (within each
#'  committee). Areas are colored as based on their region. For
#'  example, if one rule has `var1 < 10`, the linear for this rule
#'  would be colored. If another rule had the complementary region
#'  of `var1 <= 10`, it would be on another line and shaded a
#'  different color.
#' 
#'   For the coefficient plot, another dotplot is made. The layout
#'  is the same except the the x-axis is in the original units and
#'  has a dot if the rule used that variable in a linear model.
#' 
#' @param x a [cubist()] object
#' @param data not currently used (here for lattice compatibility)
#' @param what either "splits" or "coefs"
#' @param committee which committees to plot
#' @param rule which rules to plot
#' @param \dots options to pass to [lattice::dotplot()]
#' @return a [lattice::dotplot()] object
#' @author R code by Max Kuhn, original C sources by R Quinlan and
#' modifications be Steve Weston
#' @seealso [cubist()], [cubistControl()],
#' [predict.cubist()], [summary.cubist()],
#' [predict.cubist()], [lattice::dotplot()]
#' @references Quinlan. Learning with continuous classes.
#'  Proceedings of the 5th Australian Joint Conference On Artificial
#'  Intelligence (1992) pp. 343-348
#' 
#'   Quinlan. Combining instance-based and model-based learning.
#'  Proceedings of the Tenth International Conference on Machine
#'  Learning (1993) pp. 236-243
#' 
#'   Quinlan. \strong{C4.5: Programs For Machine Learning} (1993)
#'  Morgan Kaufmann Publishers Inc. San Francisco, CA
#' 
#' \url{http://rulequest.com/cubist-info.html}
#' @keywords hplot
#' @examples
#' 
#' library(mlbench)
#' data(BostonHousing)
#' 
#' ## 1 committee and no instance-based correction, so just an M5 fit:
#' mod1 <- cubist(x = BostonHousing[, -14], y = BostonHousing$medv)
#' dotplot(mod1, what = "splits")
#' dotplot(mod1, what = "coefs")
#' 
#' ## Now with 10 committees
#' mod2 <- cubist(x = BostonHousing[, -14], 
#'                y = BostonHousing$medv, 
#'                committees = 10)
#' dotplot(mod2, scales = list(y = list(cex = .25)))
#' dotplot(mod2, what = "coefs", 
#'         between = list(x = 1, y = 1),
#'         scales = list(x = list(relation = "free"), 
#'                       y = list(cex = .25)))
#' 
#' @export 
#' @method dotplot cubist
#' @importFrom lattice dotplot panel.segments trellis.par.get
#' @importFrom reshape2 melt
#' @importFrom stats complete.cases
dotplot.cubist <- function(x, data = NULL, what = "splits", 
                           committee = NULL, rule = NULL, ...) {

  splits <- x$splits
  if (is.null(splits))
    stop("No splits were used in this model", call. = FALSE)
  
  if (!is.null(committee))
    splits <- splits[splits$committee <= committee, ]
  if (!is.null(rule))
    splits <-  splits[splits$rule <= rule, ]
  
  if (max(splits$committee) == 1) {
    lab <- "Rule"
    splits$label <-
      gsub(" ", "0", format(as.character(splits$rule), justify = "right"))
  } else {
    splits$label <-
      paste(gsub(" ", "0", format(
        as.character(splits$committe), justify = "right"
      )),
      gsub(" ", "0", as.character(format(splits$rule), justify = "right")),
      sep = "/")
    lab <- "Committe/Rule"
  }

  
  if (what == "splits") {
    if (all(splits$type == "type3"))
      stop("No splits of continuous predictors were made", call. = FALSE)
    out <- dotplot(
      label ~ percentile | variable,
      data = splits,
      subset = type == "type2",
      groups = dir,
      panel = function(x, y, groups, subscripts) {
        plotTheme <- trellis.par.get()
        y2 <- as.numeric(y)
        groups <- groups[subscripts]
        isLower <- grepl("<", groups)
        for (i in seq(along = levels(y))) {
          panel.segments(
            0, i, 1, i,
            col =  plotTheme$reference.line$col,
            lty =  plotTheme$reference.line$lty,
            lwd =  plotTheme$reference.line$lwd
          )
        }
        for (i in seq(along = x)) {
          if (isLower[i]) {
            panel.segments(
              0, y2[i], x[i], y2[i],
              col = plotTheme$superpose.line$col[1],
              lty = plotTheme$superpose.line$lty[1],
              lwd = plotTheme$superpose.line$lwd[1]
            )
          } else
            panel.segments(
              x[i], y2[i], 1, y2[i],
              col = plotTheme$superpose.line$col[2],
              lty = plotTheme$superpose.line$lty[2],
              lwd = plotTheme$superpose.line$lwd[2]
            )
        }
      },
      xlim = c(-.05, 1.05),
      xlab = "Training Data Coverage",
      ylab = lab,
      ...
    )
  }
  if (what == "coefs") {
    coefVals <- x$coefficients
    coefVals <- melt(coefVals, id.vars = c("committee", "rule"))
    coefVals <- coefVals[complete.cases(coefVals), ]
    if (max(coefVals$committee) == 1) {
      lab <- "Rule"
      coefVals$label <-
        gsub(" ", "0", format(as.character(coefVals$rule), justify = "right"))
    } else {
      coefVals$label <-
        paste(gsub(" ", "0", format(
          as.character(coefVals$committe), justify = "right"
        )),
        gsub(" ", "0", as.character(format(coefVals$rule), justify = "right")),
        sep = "/")
      lab <- "Committe/Rule"
    }
    out <- dotplot(label ~ value | variable, data = coefVals, ...)
  }
  out
}

#' @importFrom utils globalVariables
utils::globalVariables(c("type"))