# Extracted from test-edge-cases.R:223

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "Cubist", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
set.seed(123)
x <- data.frame(rnorm(50), rnorm(50))
names(x) <- c("var%one", "var%two")
y <- x$`var%one` + x$`var%two` + rnorm(50, sd = 0.1)
expect_snapshot({
    mod <- cubist(x, y)
  })
