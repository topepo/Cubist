\name{cubist.default}
\alias{cubist}
\alias{cubist.default}
\title{
Fit a Cubist model
}
\description{
This function fits the rule-based model described in Quinlan (1992) (aka M5) with additional corrections based on nearest neighbors in the training set,  as described in Quinlan (1993a).

}
\usage{
cubist(x, ...)

\method{cubist}{default}(x, y, 
       committees = 1,
       control = cubistControl(), ...)
}
\arguments{
  \item{x}{
a matrix or data frame of predictor variables. Missing data are allowed but (at this time) only numeric, character and factor values are allowed.
}
  \item{y}{
a numeric vector of outcome
}
  \item{committees}{
an integer: how many committee models (e.g.. boosting iterations) should be used?
}
  \item{control}{
options that control details of the \code{cubist} algorithm. See \code{\link{cubistControl}}
}
  \item{\dots}{
optional arguments to pass (not currently used)
}
}
\details{
Cubist is a prediction-oriented regression model that combines the ideas in Quinlan (1992) and Quinlan (1993). 

Although it initially creates a tree structure, it collapses each path through the tree into a rule. A regression model is fit for each rule based on the data subset defined by the rules. The set of rules are pruned or possibly combined. and the candidate variables for the linear regression models are the predictors that were used in the parts of the rule that were pruned away. This part of the algorithm is consistent with the "M5" or Model Tree approach.

Cubist genralizes this model to add boosting (when \code{committees > 1}) and instance based corrections (see \code{\link{predict.cubist}}). The number of instances is set at prediciton time by the user and is not needed for model building.

This function links R to the GPL version of the C code given on the RuleQuest webiste.

The RuleQuest code differentiates missing values from values that are not applicable. Currently, this packages does not make such a distinction (all values are treated as missing). This will produce slightly different results.

To tune the cubist model over the number of committees and neighbors, the \code{\link[caret]{train}} function in the \pkg{caret} package has bindings to find appropriate settings of these parameters.

}
\value{
an object of class \code{cubist} with elements:
  \item{data, names, model}{character strings that correspond to their counterparts for the command-line program available from RuleQuest}
  \item{output}{basic cubist output captured from the C code, including the rules, their terminal models and variable usage statistics}
  \item{control}{a list of control parameters passed in by the user}
  \item{composite, neighbors, committees}{mirrors of the values to these arguments that were passed in by the user}
  \item{dims}{the output if \code{dim(x)}}
  \item{splits}{information about the variables and values used in the rule conditions}
  \item{call}{the function call}
  \item{coefs}{a data frame of regression coefficients for each rule within each committee}
  \item{vars}{a list with elements \code{all} and \code{used} listing the predictors passed into the function and used by any rule or model}
  \item{fitted.values}{a numeric vector of predictions on the training set.}
  \item{usage}{a data frame with the percent of models where each variable was used. See  \code{\link{summary.cubist}} for a discussion.}
}
\references{
Quinlan. Learning with continuous classes. Proceedings of the 5th Australian Joint Conference On Artificial Intelligence (1992) pp. 343-348

Quinlan. Combining instance-based and model-based learning. Proceedings of the Tenth International Conference on Machine Learning (1993a) pp. 236-243

Quinlan. \strong{C4.5: Programs For Machine Learning} (1993b) Morgan Kaufmann Publishers Inc. San Francisco, CA

Wang and Witten. Inducing model trees for continuous classes. Proceedings of the Ninth European Conference on Machine Learning (1997) pp. 128-137

\url{http://rulequest.com/cubist-info.html}

}
\author{
R code by Max Kuhn, original C sources by R Quinlan and modifications be Steve Weston
}

\seealso{
\code{\link{cubistControl}}, \code{\link{predict.cubist}}, \code{\link{summary.cubist}}, \code{\link{dotplot.cubist}}, \code{\link[caret]{train}}
}
\examples{
library(mlbench)
data(BostonHousing)

## 1 committee, so just an M5 fit:
mod1 <- cubist(x = BostonHousing[, -14], y = BostonHousing$medv)
mod1

## Now with 10 committees
mod2 <- cubist(x = BostonHousing[, -14], y = BostonHousing$medv, committees = 10)
mod2
}

\keyword{models}
