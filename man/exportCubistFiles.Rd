\name{exportCubistFiles}
\alias{exportCubistFiles}
\title{
Export Cubist Information To the File System
}
\description{
For a fitted cubist object, text files consistent with teh RuleQuest command-line version can be exported.
}
\usage{
exportCubistFiles(x, neighbors = 0, path = getwd(), prefix = NULL)
}

\arguments{
  \item{x}{
a \code{\link{cubist}} object
}
  \item{neighbors}{
how many, if any, neighbors should be used to correct the model predictions
}
  \item{path}{
the path to put the files
}
  \item{prefix}{
a prefix (or "filestem") for creating files
}
}
\details{
Using the RuleQuest specifications, \code{model}, \code{names} and \code{data} files are created for use with the command-line version of the program.

}
\value{
No value is returned. Three files are written out.
}
\references{
Quinlan. Learning with continuous classes. Proceedings of the 5th Australian Joint Conference On Artificial Intelligence (1992) pp. 343-348

Quinlan. Combining instance-based and model-based learning. Proceedings of the Tenth International Conference on Machine Learning (1993) pp. 236-243

Quinlan. \strong{C4.5: Programs For Machine Learning} (1993) Morgan Kaufmann Publishers Inc. San Francisco, CA

\url{http://rulequest.com/cubist-info.html}

}
\author{
Max Kuhn
}

\seealso{
\code{\link{cubist}}\code{\link{cubistControl}}, \code{\link{predict.cubist}}, \code{\link{summary.cubist}}, \code{\link{predict.cubist}}
}
\examples{
library(mlbench)
data(BostonHousing)

mod1 <- cubist(x = BostonHousing[, -14], y = BostonHousing$medv)
exportCubistFiles(mod1, neighbors = 8, path = tempdir(), prefix = "BostonHousing")

}

\keyword{models}
