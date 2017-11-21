#' Export Cubist Information To the File System
#' 
#' For a fitted cubist object, text files consistent with the
#'  RuleQuest command-line version can be exported.
#' 
#' Using the RuleQuest specifications, `model`, `names` and `data`
#'  files are created for use with the command-line version of the
#'  program.
#' 
#' @param x a [cubist()] object
#' @param neighbors how many, if any, neighbors should be used to
#'  correct the model predictions
#' @param path the path to put the files
#' @param prefix a prefix (or "filestem") for creating files
#' @return No value is returned. Three files are written out.
#' @author Max Kuhn
#' @seealso [cubist()][cubistControl()],
#' [predict.cubist()], [summary.cubist()],
#' [predict.cubist()]
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
#' @keywords models
#' @examples
#' 
#' library(mlbench)
#' data(BostonHousing)
#' 
#' mod1 <- cubist(x = BostonHousing[, -14], y = BostonHousing$medv)
#' exportCubistFiles(mod1, neighbors = 8, path = tempdir(), prefix = "BostonHousing")
#' 
#' 
#' @export exportCubistFiles
exportCubistFiles <-
  function(x,
           neighbors = 0,
           path = getwd(),
           prefix = NULL) {
    if (neighbors > 0) {
      x$model <- gsub(
        "insts=\"0\"",
        paste(
          "insts=\"1\" nn=\"",
          neighbors,
          "\" maxd=\"",
          x$maxd,
          "\"",
          sep = ""
        ),
        x$model
      )
    }
    if (is.null(prefix))
      prefix <-
        paste("model", format(Sys.time(), "%Y%m%d_%H%M"), sep = "")
    cat(x$model, file = file.path(path, paste(prefix, "model", sep = ".")))
    modelTest <-
      file.exists(file.path(path, paste(prefix, "model", sep = ".")))
    if (!modelTest)
      stop("the model file could not be created")
    
    cat(x$names, file = file.path(path, paste(prefix, "names", sep = ".")))
    namesTest <-
      file.exists(file.path(path, paste(prefix, "names", sep = ".")))
    if (!namesTest)
      stop("the names file could not be created")
    
    cat(x$data, file = file.path(path, paste(prefix, "data", sep = ".")))
    dataTest <-
      file.exists(file.path(path, paste(prefix, "data", sep = ".")))
    if (!dataTest)
      stop("the data file could not be created")
  }
