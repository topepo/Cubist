#' Rule-based regression modeling
#'
#' Cubist implements Quinlan's rule-based regression model. It builds
#' predictive models by growing a tree structure and collapsing each path into
#' a rule, then fitting a linear regression model for each rule based on the
#' data subset it covers. Predictions can optionally be improved using boosting
#' via committees (ensembles of rule sets) and nearest-neighbor adjustments
#' from training instances.
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import rlang
#' @importFrom cli cli_abort
#' @importFrom lattice dotplot panel.segments trellis.par.get
#' @importFrom reshape2 melt
#' @importFrom stats complete.cases reshape
#' @importFrom utils globalVariables
## usethis namespace: end

## mockable bindings: start
## mockable bindings: end
NULL

utils::globalVariables(c("type"))
