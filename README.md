
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/Cubist)](https://CRAN.R-project.org/package=Cubist)
[![Downloads](http://cranlogs.r-pkg.org/badges/Cubist)](https://CRAN.r-project.org/package=Cubist)
![](https://img.shields.io/badge/lifecycle-maturing-blue.svg)
[![R-CMD-check](https://github.com/topepo/Cubist/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/topepo/Cubist/actions/workflows/R-CMD-check.yaml)

The `Cubist` R package fits Quinlan's Cubist regression model based on the source of from [`www.rulequest.com`](http://www.rulequest.com/cubist-info.html). Some of the functionality is based on 

*  Quinlan. [Learning with continuous classes](https://scholar.google.com/scholar?hl=en&as_sdt=0%2C7&q=%22Learning+with+continuous+classes%22&btnG=). _Proceedings of the 5th Australian Joint Conference On Artificial Intelligence_ (1992) pp. 343-348

* Quinlan. [Combining instance-based and model-based learning](https://scholar.google.com/scholar?hl=en&as_sdt=0%2C7&q=%22Combining+instance-based+and+model-based+learning%22&btnG=). _Proceedings of the Tenth International Conference on Machine Learning_ (1993) pp. 236-243

More details on Cubist can be found in [_Applied Predictive Modeling_](http://appliedpredictivemodeling.com/). A presentation on the model can be found [here](https://www.dropbox.com/s/2vf3swfbk48lfdc/RulesRulesRules.pdf?dl=0). This [R Views blog post](https://rviews.rstudio.com/2020/05/21/modern-rule-based-models/) has information on Cubist and compares it to RuleFit. 

To install the production version of the package, use:

```r
install.packages("Cubist")
```

and to install the development version, use

```r
require("devtools")
install_github("topepo/Cubist")
```
