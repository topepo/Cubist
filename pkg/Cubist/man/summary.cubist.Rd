\name{summary.cubist}
\alias{summary.cubist}
\title{
Summarizing Cubist Fits
}
\description{
a \code{summary} method for \code{\link{cubist}} objects
}
\usage{
\method{summary}{cubist}(object, ...)
}
\arguments{
  \item{object}{
a \code{\link{cubist}} object
}
  \item{\dots}{
other options (not currently used)
}
}
\details{
This function echos the output of the RuleQuest C code, including the rules, the resulting linear models as well as the variable usage summaries.

The Cubist output contains variable usage statistics. It gives the percentage of times where each variable was used in a condition and/or a linear model. Note that this output will probably be inconsistent with the rules shown above. At each split of the tree, Cubist saves a linear model (after feature selection) that is allowed to have terms for each variable used in the current split or any split above it. Quinlan (1992) discusses a smoothing algorithm where each model prediction is a linear combination of the parent and child model along the tree. As such, the final prediction is a function of all the linear models from the initial node to the terminal node. The percentages shown in the Cubist output reflects all the models involved in prediction (as opposed to the terminal models shown in the output).


}
\value{
an object of class \code{summary.cubist} with elements
  \item{output }{a text string of the output}
  \item{call }{the original call to \code{\link{cubist}}}
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
\code{\link{cubist}}, \code{\link{cubistControl}}, \code{\link{predict.cubist}}, \code{\link{dotplot.cubist}}
}
\examples{
library(mlbench)
data(BostonHousing)

## 1 committee and no instance-based correction, so just an M5 fit:
mod1 <- cubist(x = BostonHousing[, -14], y = BostonHousing$medv)
summary(mod1)

## example output:

## Cubist [Release 2.07 GPL Edition]  Sun Apr 10 17:36:56 2011
## ---------------------------------
## 
##     Target attribute `outcome'
## 
## Read 506 cases (14 attributes) from undefined.data
## 
## Model:
## 
##   Rule 1: [101 cases, mean 13.84, range 5 to 27.5, est err 1.98]
## 
##     if
##     nox > 0.668
##     then
##     outcome = -1.11 + 2.93 dis + 21.4 nox - 0.33 lstat + 0.008 b
##               - 0.13 ptratio - 0.02 crim - 0.003 age + 0.1 rm
## 
##   Rule 2: [203 cases, mean 19.42, range 7 to 31, est err 2.10]
## 
##     if
##     nox <= 0.668
##     lstat > 9.59
##     then
##     outcome = 23.57 + 3.1 rm - 0.81 dis - 0.71 ptratio - 0.048 age
##               - 0.15 lstat + 0.01 b - 0.0041 tax - 5.2 nox + 0.05 crim
##               + 0.02 rad
## 
##   Rule 3: [43 cases, mean 24.00, range 11.9 to 50, est err 2.56]
## 
##     if
##     rm <= 6.226
##     lstat <= 9.59
##     then
##     outcome = 1.18 + 3.83 crim + 4.3 rm - 0.06 age - 0.11 lstat - 0.003 tax
##               - 0.09 dis - 0.08 ptratio
## 
##   Rule 4: [163 cases, mean 31.46, range 16.5 to 50, est err 2.78]
## 
##     if
##     rm > 6.226
##     lstat <= 9.59
##     then
##     outcome = -4.71 + 2.22 crim + 9.2 rm - 0.83 lstat - 0.0182 tax
##               - 0.72 ptratio - 0.71 dis - 0.04 age + 0.03 rad - 1.7 nox
##               + 0.008 zn
## 
## 
## Evaluation on training data (506 cases):
## 
##     Average  |error|               2.07
##     Relative |error|               0.31
##     Correlation coefficient        0.94
## 
## 
##     Attribute usage:
##       Conds  Model
## 
##        80%   100%    lstat
##        60%    92%    nox
##        40%   100%    rm
##              100%    crim
##              100%    age
##              100%    dis
##              100%    ptratio
##               80%    tax
##               72%    rad
##               60%    b
##               32%    zn
## 
## 
## Time: 0.0 secs

}

\keyword{models}
