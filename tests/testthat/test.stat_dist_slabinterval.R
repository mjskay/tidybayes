# Tests for analytical distribution plots
#
# Author: mjskay
###############################################################################

library(dplyr)



test_that("distribution eye plots work with the args aesthetic", {
  skip_if_no_vdiffr()


  p = tribble(
    ~dist, ~args,
    "norm", list(0, 1),
    "beta", list(5, 5),
    NA, NA
  ) %>%
    ggplot(aes(dist = dist, args = args))

  # test deprecation warning
  expect_warning(vdiffr::expect_doppelganger("horizontal half-eye using args",
    p + stat_dist_halfeyeh(aes(y = dist), na.rm = TRUE, n = 40)
  ), "Deprecated")

  # test deprecation warning
  expect_warning(vdiffr::expect_doppelganger("ccdfintervalh using args",
    p + stat_dist_ccdfintervalh(aes(y = dist), na.rm = TRUE, n = 40)
  ), "Deprecated")

})
