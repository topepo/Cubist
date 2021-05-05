#' @export
cubist <-  function(x, ...) UseMethod("cubist")

# About the Cubist C code and our approach here...

# 1) The cubist code is written to take specific data files from
#  the file system, pull them into memory, run the computations,
#  then write the results to a text file that is also saved to the
#  file system.

# 2) The code makes use of a lot of global variables (especially
#  for the data)

# 3). The code has been around for a while and, after reading it,
#  one can tell that the author put in a lot of time to catch many
#  special cases. We have pushed millions of samples through the
#  code without any errors

# So... the approach here is to pass in the training data as
#  strings that mimic the formats that one would use with the
#  command line version and get back the textual representation
#  that would be saved to the .model file also as a string. The
#  predicton function would then pass the model text string (and
#  the data text string if instances are used) to the C code for
#  prediction.

# We did this for a few reasons:

# a) this approach would require us to re-write main() and touch
#  as little of the original code as possible (otherwise we would
#  have to write a parser for the data and try to get it into the
#  global variable structure with complete fidelity)

# b) most modeling functions implicitly assume that the data
#  matrix is all numeric, thus factors are converted to dummy
#  variables etc. Cubist doesn't want categorical data split into
#  dummy variables based on how it does splits. Thus, we would have
#  to pass in the numeric and categorical predictors separately
#  unless we want to get really fancy.


#' Fit a Cubist model
#'
#' This function fits the rule-based model described in Quinlan
#'  (1992) (aka M5) with additional corrections based on nearest
#'  neighbors in the training set, as described in Quinlan (1993a).
#'
#' Cubist is a prediction-oriented regression model that combines
#'  the ideas in Quinlan (1992) and Quinlan (1993).
#'
#' Although it initially creates a tree structure, it collapses
#'  each path through the tree into a rule. A regression model is
#'  fit for each rule based on the data subset defined by the rules.
#'  The set of rules are pruned or possibly combined. and the
#'  candidate variables for the linear regression models are the
#'  predictors that were used in the parts of the rule that were
#'  pruned away. This part of the algorithm is consistent with the
#'  "M5" or Model Tree approach.
#'
#' Cubist generalizes this model to add boosting (when
#'  `committees > 1`) and instance based corrections (see
#'  [predict.cubist()]). The number of instances is set at
#'  prediction time by the user and is not needed for model
#'  building.
#'
#' This function links R to the GPL version of the C code given on
#'  the RuleQuest website.
#'
#' The RuleQuest code differentiates missing values from values
#'  that are not applicable. Currently, this packages does not make
#'  such a distinction (all values are treated as missing). This
#'  will produce slightly different results.
#'
#' To tune the cubist model over the number of committees and
#'  neighbors, the [caret::train()] function in the `caret` package
#'  has bindings to find appropriate settings of these parameters.
#'
#' @aliases cubist cubist.default
#' @param x a matrix or data frame of predictor variables. Missing
#'  data are allowed but (at this time) only numeric, character and
#'  factor values are allowed. Must have column names.
#' @param y a numeric vector of outcome
#' @param committees an integer: how many committee models (e.g..
#'  boosting iterations) should be used?
#' @param control options that control details of the `cubist`
#'  algorithm. See [cubistControl()]
#' @param weights an optional vector of case weights (the same
#'  length as `y`) for how much each instance should contribute to
#'  the model fit. From the RuleQuest website: "The relative
#'  weight assigned to each case is its value of this attribute
#'  divided by the average value; if the value is undefined, not
#'  applicable, or is less than or equal to zero, the case's
#'  relative weight is set to 1."
#' @param \dots optional arguments to pass (not currently used)
#' @return an object of class `cubist` with elements:
#'  \item{data, names, model}{character strings that correspond to
#'  their counterparts for the command-line program available from
#'  RuleQuest}
#'  \item{output}{basic cubist output captured from the C code,
#'  including the rules, their terminal models and variable usage
#'  statistics}
#'  \item{control}{a list of control parameters passed in by the
#'  user}
#'  \item{composite, neighbors, committees}{mirrors of the values to
#'  these arguments that were passed in by the user}
#'  \item{dims}{the output if `dim(x)`}
#'  \item{splits}{information about the variables and values used in
#'  the rule conditions}
#'  \item{call}{the function call}
#'  \item{coefs}{a data frame of regression coefficients for each
#'  rule within each committee}
#'  \item{vars}{a list with elements `all` and `used` listing the
#'  predictors passed into the function and used by any rule or
#'  model}
#'  \item{fitted.values}{a numeric vector of predictions on the
#'  training set.}
#'  \item{usage}{a data frame with the percent of models where each
#'  variable was used. See [summary.cubist()] for a discussion.}
#' @author R code by Max Kuhn, original C sources by R Quinlan and
#'  modifications be Steve Weston
#' @seealso [cubistControl()], [predict.cubist()],
#'  [summary.cubist()], [dotplot.cubist()], [caret::train()]
#' @references Quinlan. Learning with continuous classes.
#'  Proceedings of the 5th Australian Joint Conference On Artificial
#'  Intelligence (1992) pp. 343-348
#'
#'   Quinlan. Combining instance-based and model-based learning.
#'  Proceedings of the Tenth International Conference on Machine
#'  Learning (1993a) pp. 236-243
#'
#'   Quinlan. \strong{C4.5: Programs For Machine Learning} (1993b)
#'  Morgan Kaufmann Publishers Inc. San Francisco, CA
#'
#'   Wang and Witten. Inducing model trees for continuous classes.
#'  Proceedings of the Ninth European Conference on Machine Learning
#'  (1997) pp. 128-137
#'
#' \url{http://rulequest.com/cubist-info.html}
#' @keywords models
#' @useDynLib Cubist
#' @examples
#'
#' library(mlbench)
#' data(BostonHousing)
#'
#' ## 1 committee, so just an M5 fit:
#' mod1 <- cubist(x = BostonHousing[, -14], y = BostonHousing$medv)
#' mod1
#'
#' ## Now with 10 committees
#' mod2 <- cubist(x = BostonHousing[, -14], y = BostonHousing$medv, committees = 10)
#' mod2
#'
#' @export
#' @method cubist default
cubist.default <- function(x, y,
                           committees = 1,
                           control = cubistControl(),
                           weights = NULL,
                           ...) {
  funcCall <- match.call(expand.dots = TRUE)
  if (!is.numeric(y))
    stop("cubist models require a numeric outcome", call. = FALSE)

  if (committees < 1 | committees > 100)
    stop("number of committees must be between 1 and 100", call. = FALSE)

  if (!is.data.frame(x) & !is.matrix(x))
    stop("x must be a matrix or data frame", call. = FALSE)
  if (inherits(x, "tbl_df")) {
    x <- as.data.frame(x)
  }

  if (!is.null(weights) && !is.numeric(weights))
    stop("case weights must be numeric", call. = FALSE)

  check_names(x)
  namesString <-
    makeNamesFile(x, y, w = weights, label = control$label, comments = TRUE)
  dataString <- makeDataFile(x, y, weights)

  Z <- .C("cubist",
          as.character(namesString),
          as.character(dataString),
          as.logical(control$unbiased),     # -u : generate unbiased rules
          "yes",                            # -i and -a : how to combine these?
          as.integer(1),                    # -n : set the number of nearest neighbors (1 to 9)
          as.integer(committees),           # -c : construct a committee model
          as.double(control$sample),        # -S : use a sample of x% for training
                                            #      and a disjoint sample for testing
          as.integer(control$seed),         # -I : set the sampling seed value
          as.integer(control$rules),        # -r: set the maximum number of rules
          as.double(control$extrapolation), # -e : set the extrapolation limit
          model = character(1),             # pass back .model file as a string
          output = character(1),            # pass back cubist output as a string
          PACKAGE = "Cubist"
          )

  # Check for converted names
  has_reserved <- grep("\n__Sample", namesString, fixed = TRUE)
  if(length(has_reserved) > 0) {
    Z$output <- gsub("__Sample", "sample", Z$output)
    Z$model <- gsub("__Sample", "sample", Z$model)
  }

  splits <- getSplits(Z$model)
  if (!is.null(splits)) {
    splits$percentile <- NA
    for (i in 1:nrow(splits)) {
      if (!is.na(splits$value[i]))
        splits$percentile[i] <-
          sum(x[, as.character(splits$variable[i])] <= splits$value[i]) / nrow(x)
    }
  }

  tmp <- strsplit(Z$model, "\\n")[[1]]
  tmp <- tmp[grep("maxd", tmp)]
  tmp <- strsplit(tmp, "\"")[[1]]
  maxd <- tmp[grep("maxd", tmp) + 1]
  Z$model <-
    gsub(paste("insts=\"1\" nn=\"1\" ", "maxd=\"", maxd, "\"", sep = ""),
         "insts=\"0\"",
         Z$model)
  maxd <- as.double(maxd)

  usage <- varUsage(Z$output)
  if (is.null(usage) || nrow(usage) < ncol(x)) {
    check_names(x)
    xNames <- colnames(x)

    uNames <-
      if (!is.null(usage))
        as.character(usage$Variable)
    else
      ""
    if (!all(xNames %in% uNames)) {
      usage2 <- data.frame(Conditions = 0,
                           Model = 0,
                           Variable = xNames[!(xNames %in% uNames)])
      usage <- rbind(usage, usage2)
    }
  }


  out <- list(data = dataString,
              names = namesString,
              caseWeights = !is.null(weights),
              model = Z$model,
              output = Z$output,
              control = control,
              committees = committees,
              maxd = maxd,
              dims = dim(x),
              splits = splits,
              usage = usage,
              call = funcCall)
  coefs <- coef.cubist(out, varNames = colnames(x))
  out$coefficients <- coefs

  tmp <-
    apply(coefs[,-(1:3), drop = FALSE], 2,
          function(x) any(!is.na(x)))
  tmp <- names(tmp)[tmp]
  xInfo <- list(all = colnames(x),
                used = union(as.character(splits$variable), tmp))

  out$vars <- xInfo
  class(out) <- "cubist"
  out
}




#' Various parameters that control aspects of the Cubist fit.
#'
#' Most of these values are discussed at length in
#'  \url{http://rulequest.com/cubist-unix.html}
#'
#' @param unbiased a logical: should unbiased rules be used?
#' @param rules an integer (or `NA`): define an explicit limit to
#'  the number of rules used (`NA` let's Cubist decide).
#' @param extrapolation a number between 0 and 100: since Cubist
#'  uses linear models, predictions can be outside of the outside of
#'  the range seen the training set. This parameter controls how
#'  much rule predictions are adjusted to be consistent with the
#'  training set.

#' @param sample a number between 0 and 99.9: this is the
#'  percentage of the data set to be randomly selected for model
#'  building (not for out-of-bag type evaluation).
#' @param seed an integer for the random seed (in the C code)
#' @param label a label for the outcome (when printing rules)
#' @return A list containing the options.
#' @author Max Kuhn
#' @seealso [cubist()], [predict.cubist()], [summary.cubist()],
#'  [predict.cubist()], [dotplot.cubist()]

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
#' @keywords utilities
#' @examples
#'
#' cubistControl()
#'
#' @export cubistControl
cubistControl <- function(
  unbiased = FALSE,
  rules = 100,
  extrapolation = 100,
  sample = 0.0,
  seed = sample.int(4096, size=1) - 1L,
  label = "outcome"
) {
  if (!is.na(rules) & (rules < 1 | rules > 1000000))
    stop("number of rules must be between 1 and 1000000", call. = FALSE)
  if (extrapolation < 0 | extrapolation > 100)
    stop("percent extrapolation must between 0 and 100", call. = FALSE)
  if (sample < 0.0 | sample > 99.9)
    stop("sampling percentage must be between 0.0 and 99.9", call. = FALSE)

  list(
    unbiased = unbiased,
    rules = rules,
    extrapolation = extrapolation / 100,
    sample = sample / 100,
    label = label,
    seed = seed %% 4096L
  )
  }


#' @export
print.cubist <- function(x, ...) {
  cat("\nCall:\n", truncateText(deparse(x$call, width.cutoff = 500)), "\n\n", sep = "")


  nRules <- countRules(x$model)

  cat("Number of samples:",
      x$dims[1],
      "\nNumber of predictors:",
      x$dims[2],
      "\n\n")

  if(x$caseWeights)
    cat("Case weights used\n\n")

  cat("Number of committees:", length(nRules), "\n")
  if (length(nRules) > 1) {
    ruleText <-
      if (length(nRules) > 20)
        paste(paste(nRules[1:20], collapse = ", "), "...")
    else
      paste(nRules, collapse = ", ")
    cat("Number of rules per committee:", ruleText, "\n")
  } else
    cat("Number of rules:", nRules, "\n")
  otherOptions <- NULL
  if (x$control$unbiased)
    otherOptions <- c(otherOptions, "unbiased rules")
  if (x$control$extrapolation < 1)
    otherOptions <- c(otherOptions,
                      paste(
                        round(x$control$extrapolation * 100, 1),
                        "% extrapolation",
                        sep = ""
                      ))
  if (x$control$sample > 0)
    otherOptions <- c(otherOptions,
                      paste(round(100 * x$control$sample, 1),
                            "% sub-sampling", sep = ""))
  if (!is.null(otherOptions))
    cat("Other options:", paste(otherOptions, collapse = ", "))
  cat("\n")
}

#' Summarizing Cubist Fits
#'
#'
#' This function echoes the output of the RuleQuest C code,
#'  including the rules, the resulting linear models as well as the
#'  variable usage summaries.
#'
#' The Cubist output contains variable usage statistics. It gives
#'  the percentage of times where each variable was used in a
#'  condition and/or a linear model. Note that this output will
#'  probably be inconsistent with the rules shown above. At each
#'  split of the tree, Cubist saves a linear model (after feature
#'  selection) that is allowed to have terms for each variable used
#'  in the current split or any split above it. Quinlan (1992)
#'  discusses a smoothing algorithm where each model prediction is a
#'  linear combination of the parent and child model along the tree.
#'  As such, the final prediction is a function of all the linear
#'  models from the initial node to the terminal node. The
#'  percentages shown in the Cubist output reflects all the models
#'  involved in prediction (as opposed to the terminal models shown
#'  in the output).
#'
#' @param object a [cubist()] object
#' @param \dots other options (not currently used)
#' @return an object of class `summary.cubist` with elements
#'  \item{output }{a text string of the output}
#'  \item{call }{the original call to [cubist()]}
#' @author R code by Max Kuhn, original C sources by R Quinlan and
#'  modifications be Steve Weston
#' @seealso [cubist()], [cubistControl()], [predict.cubist()],
#'  [dotplot.cubist()]
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
#' ## 1 committee and no instance-based correction, so just an M5 fit:
#' mod1 <- cubist(x = BostonHousing[, -14], y = BostonHousing$medv)
#' summary(mod1)
#'
#' ## example output:
#'
#' ## Cubist [Release 2.07 GPL Edition]  Sun Apr 10 17:36:56 2011
#' ## ---------------------------------
#' ##
#' ##     Target attribute `outcome'
#' ##
#' ## Read 506 cases (14 attributes) from undefined.data
#' ##
#' ## Model:
#' ##
#' ##   Rule 1: [101 cases, mean 13.84, range 5 to 27.5, est err 1.98]
#' ##
#' ##     if
#' ##     nox > 0.668
#' ##     then
#' ##     outcome = -1.11 + 2.93 dis + 21.4 nox - 0.33 lstat + 0.008 b
#' ##               - 0.13 ptratio - 0.02 crim - 0.003 age + 0.1 rm
#' ##
#' ##   Rule 2: [203 cases, mean 19.42, range 7 to 31, est err 2.10]
#' ##
#' ##     if
#' ##     nox <= 0.668
#' ##     lstat > 9.59
#' ##     then
#' ##     outcome = 23.57 + 3.1 rm - 0.81 dis - 0.71 ptratio - 0.048 age
#' ##               - 0.15 lstat + 0.01 b - 0.0041 tax - 5.2 nox + 0.05 crim
#' ##               + 0.02 rad
#' ##
#' ##   Rule 3: [43 cases, mean 24.00, range 11.9 to 50, est err 2.56]
#' ##
#' ##     if
#' ##     rm <= 6.226
#' ##     lstat <= 9.59
#' ##     then
#' ##     outcome = 1.18 + 3.83 crim + 4.3 rm - 0.06 age - 0.11 lstat - 0.003 tax
#' ##               - 0.09 dis - 0.08 ptratio
#' ##
#' ##   Rule 4: [163 cases, mean 31.46, range 16.5 to 50, est err 2.78]
#' ##
#' ##     if
#' ##     rm > 6.226
#' ##     lstat <= 9.59
#' ##     then
#' ##     outcome = -4.71 + 2.22 crim + 9.2 rm - 0.83 lstat - 0.0182 tax
#' ##               - 0.72 ptratio - 0.71 dis - 0.04 age + 0.03 rad - 1.7 nox
#' ##               + 0.008 zn
#' ##
#' ##
#' ## Evaluation on training data (506 cases):
#' ##
#' ##     Average  |error|               2.07
#' ##     Relative |error|               0.31
#' ##     Correlation coefficient        0.94
#' ##
#' ##
#' ##     Attribute usage:
#' ##       Conds  Model
#' ##
#' ##        80%   100%    lstat
#' ##        60%    92%    nox
#' ##        40%   100%    rm
#' ##              100%    crim
#' ##              100%    age
#' ##              100%    dis
#' ##              100%    ptratio
#' ##               80%    tax
#' ##               72%    rad
#' ##               60%    b
#' ##               32%    zn
#' ##
#' ##
#' ## Time: 0.0 secs
#'
#'
#' @method summary cubist
#' @export
summary.cubist <- function(object, ...) {
  out <- list(output = object$output, call = object$call)
  class(out) <- "summary.cubist"
  out
}

#' @export
print.summary.cubist <- function(x, ...) {
  cat(
    "\nCall:\n",
    truncateText(deparse(x$call, width.cutoff = 500)),
    "\n\n",
    sep = ""
  )
  cat(x$output)
  cat("\n")
  invisible(x)
}

truncateText <- function(x) {
  if (length(x) > 1)
    x <- paste(x, collapse = "")
  w <- options("width")$width
  if (nchar(x) <= w)
    return(x)

  cont <- TRUE
  out <- x
  while (cont) {
    tmp <- out[length(out)]
    tmp2 <- substring(tmp, 1, w)

    spaceIndex <- gregexpr("[[:space:]]", tmp2)[[1]]
    stopIndex <- spaceIndex[length(spaceIndex) - 1] - 1
    tmp <- c(substring(tmp2, 1, stopIndex),
             substring(tmp, stopIndex + 1))
    out <-
      if (length(out) == 1)
        tmp
    else
      c(out[1:(length(x) - 1)], tmp)
    if (all(nchar(out) <= w))
      cont <- FALSE
  }

  paste(out, collapse = "\n")
}

check_names <- function(x) {
  cn <- colnames(x)
  if (is.null(cn)) {
    stop("The data should have column names")
  }
  invisible(NULL)
}
