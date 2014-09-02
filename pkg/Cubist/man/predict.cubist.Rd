\name{predict.cubist}
\alias{predict.cubist}
\title{
Predict method for cubist fits
}
\description{
Predicted values based on a cubist object.
}
\usage{
\method{predict}{cubist}(object, newdata = NULL, neighbors = 0, ...)
}
\arguments{
  \item{object}{
an object of class \code{cubist}
}
  \item{newdata}{
a data frame of predictors (in the same order as the original training data)
}

  \item{neighbors}{
an integer from 0 to 9: how many instances to use to correct the rule-based prediction?
}

  \item{\dots}{
other options to pass through the function (not currently used)
}
}
\details{
Prediction using the parametric model are calculated using the method of Quinlan (1992). If \code{neighbors} is greater than zero, these predictions are adjusted by training set instances nearby using the approach of Qunilan (1993).

}
\value{
a numeric vector is returned
}
\references{
Quinlan. Learning with continuous classes. Proceedings of the 5th Australian Joint Conference On Artificial Intelligence (1992) pp. 343-348

Quinlan. Combining instance-based and model-based learning. Proceedings of the Tenth International Conference on Machine Learning (1993) pp. 236-243

Quinlan. \strong{C4.5: Programs For Machine Learning} (1993) Morgan Kaufmann Publishers Inc. San Francisco, CA

\url{http://rulequest.com/cubist-info.html}

}

\author{
R code by Max Kuhn, original C sources by R Quinlan and modifications be Steve Weston
}

\seealso{
\code{\link{cubist}}, \code{\link{cubistControl}}, \code{\link{summary.cubist}}, \code{\link{predict.cubist}}, \code{\link{dotplot.cubist}}
}
\examples{
library(mlbench)
data(BostonHousing)

## 1 committee and no instance-based correction, so just an M5 fit:
mod1 <- cubist(x = BostonHousing[, -14], y = BostonHousing$medv)
predict(mod1, BostonHousing[1:4, -14])

## now add instances
predict(mod1, BostonHousing[1:4, -14], neighbors = 5)

}

\keyword{models}
