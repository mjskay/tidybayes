# Tests for point_interval
#
# Author: mjskay
###############################################################################

library(dplyr)
library(tidyr)




test_that("point_interval works on vectors", {
  set.seed(1234)
  x = rnorm(100, mean = 5)

  ref = data.frame(
    y = mean(x),
    ymin = as.vector(quantile(x, probs = .025)),
    ymax = as.vector(quantile(x, probs = .975)),
    .width = .95,
    .point = "mean",
    .interval = "qi",
    stringsAsFactors = FALSE
  )

  expect_warning(
    expect_equal(mean_qih(x), rename(ref, x = y, xmin = ymin, xmax = ymax))
  , "Deprecated")
})
