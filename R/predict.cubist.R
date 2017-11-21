#' Predict method for cubist fits
#' 
#' 
#'Prediction using the parametric model are calculated using the
#'  method of Quinlan (1992). If `neighbors` is greater than zero,
#'  these predictions are adjusted by training set instances nearby
#'  using the approach of Quinlan (1993).
#' 
#' @param object an object of class `cubist`
#' @param newdata a data frame of predictors (in the same order as
#'  the original training data)
#' @param neighbors an integer from 0 to 9: how many instances to
#'  use to correct the rule-based prediction?
#' @param \dots other options to pass through the function (not
#'  currently used)
#' @return a numeric vector is returned
#' @author R code by Max Kuhn, original C sources by R Quinlan and
#'  modifications be Steve Weston
#' @seealso [cubist()], [cubistControl()], [summary.cubist()],
#'  [predict.cubist()], [dotplot.cubist()]
#' @references Quinlan. Learning with continuous classes.
#'  Proceedings of the 5th Australian Joint Conference On Artificial
#'  Intelligence (1992) pp. 343-348
#' 
#' Quinlan. Combining instance-based and model-based learning.
#'  Proceedings of the Tenth International Conference on Machine
#'  Learning (1993) pp. 236-243
#' 
#' Quinlan. \strong{C4.5: Programs For Machine Learning} (1993)
#'  Morgan Kaufmann Publishers Inc. San Francisco, CA
#' 
#' \url{http://rulequest.com/cubist-info.html}
#' @keywords models
#' @examples
#' 
#' library(mlbench)
#' data(BostonHousing)
#' 
#' ## 1 committee and no instance-based correction, so just an M5 fit:
#' mod1 <- cubist(x = BostonHousing[, -14], y = BostonHousing$medv)
#' predict(mod1, BostonHousing[1:4, -14])
#' 
#' ## now add instances
#' predict(mod1, BostonHousing[1:4, -14], neighbors = 5)
#' 
#' @method predict cubist 
#' @export 
predict.cubist <- function (object, newdata = NULL, neighbors = 0, ...) {
  if (is.null(newdata))
    stop("newdata must be non-null", call. = FALSE)
  
  ## check order of data to make sure that it is the same
  newdata <- newdata[, object$vars$all, drop = FALSE]
  
  if (length(neighbors) > 1)
    stop("only a single value of neighbors is allowed")
  if (neighbors > 9)
    stop("'neighbors' must be less than 10")
  if (neighbors > 0) {
    object$model <- gsub(
      "insts=\"0\"",
      paste(
        "insts=\"1\" nn=\"",
        neighbors,
        "\" maxd=\"",
        object$maxd,
        "\"",
        sep = ""
      ),
      object$model
    )
  }
  
  ## If there are case weights used during training, the C code
  ## will expect a column of weights in the new data but the 
  ## values will be ignored. `makeDataFile` puts those last in 
  ## the data when `cubist.default` is run, so we will add a 
  ## column of NA values at the end here
  if (object$caseWeights)
    newdata$case_weight_pred <- NA
  
  ## make cases file
  caseString <- makeDataFile(x = newdata, y = NULL)
  
  ## fix breaking predictions when using sample parameter
  caseModel <- ifelse(!(regexpr("sample", object$model) == -1),
                      paste0(
                        substr(object$model, 1,
                               regexpr("sample", object$model) - 1),
                        substr(
                          object$model,
                          regexpr("entries", object$model),
                          nchar(object$model)
                        )
                      ),
                      object$model)
  
  Z <- .C("predictions",
          as.character(caseString),
          as.character(object$names),
          as.character(object$data),
          as.character(caseModel),
          pred = double(nrow(newdata)),    
          output = character(1),
          PACKAGE = "Cubist")
  
  if(all(Z$pred == 0))
    stop(Z$output, call. = FALSE)
  if(nchar(Z$output) > 0) {
    bad_att_rows <- bad_att_index(Z$output)
    if(length(bad_att_rows))
      Z$pred[bad_att_rows] <- NA
    warning(Z$output, "\n Setting to NA", call. = FALSE)
  }
  Z$pred
}

# There are occations when a new sample is predicted that has a 
# different categoriucal value than what was in the training set.
# The C code does nto return a status integer and only issues
# errors in Z$output so we parse that and set these cases to NA

bad_att_index <- function(x) {
  bad_att <- grep("\n    bad value of", x, fixed = TRUE)
  if(length(bad_att) == 0) {
    return(integer(0))
  } else {
    x <- x[bad_att]
    x <- strsplit(x, "\n")[[1]]
    x <- x[grepl("*** line", x, fixed = TRUE)]
    x <- gsub("*** line ", "", x, fixed = TRUE)
    x <- strsplit(x, " ")
    x <- lapply(x, function(x) as.integer(x[1]))
    x <- unlist(x)
  }
  x
}


