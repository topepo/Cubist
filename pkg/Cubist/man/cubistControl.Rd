\name{cubistControl}
\alias{cubistControl}
\title{
Various parameters that control aspects of the Cubist fit.
}
\description{
Various parameters that control aspects of the Cubist fit.
}
\usage{
cubistControl(unbiased = FALSE, 
              rules = 100, 
              extrapolation = 100,
              sample = 0, 
              seed = sample.int(4096, size = 1) - 1L,
              label = "outcome")
}
\arguments{
  \item{unbiased}{
a logical: should unbiased rules be used?
}
  \item{rules}{
an integer (or \code{NA}): define an explicit limit to the number of rules used (\code{NA} let's Cubist decide).
}
  \item{extrapolation}{
a number between 0 and 100: since Cubist uses linear models, predictions can be outside of the outside of the range seen the training set. This parameter controls how much rule predictions are adjusted to be consistent with the training set.  
}
  \item{sample}{
a number between 0 and 99.9: this is the percentage of the data set to be randomly selected for model building (not for out-of-bag type evaluation).
}
  \item{seed}{
an integer for the random seed (in the C code)
}
  \item{label}{
a label for the outcome (when printing rules)
}
}
\details{
Most of these values are discussed at length in \url{http://rulequest.com/cubist-unix.html}
}
\value{
A list containing the options.
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
\code{\link{cubist}}, \code{\link{predict.cubist}}, \code{\link{summary.cubist}}, \code{\link{predict.cubist}}, \code{\link{dotplot.cubist}}
}
\examples{
cubistControl()
}

\keyword{ utilities}

