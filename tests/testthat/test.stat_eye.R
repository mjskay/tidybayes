# Tests for eye plots
#
# Author: mjskay
###############################################################################

library(dplyr)
library(tidyr)



test_that("one-parameter eye plots work", {
  skip_if_no_vdiffr()
  skip_if_sensitive_to_density()


  set.seed(123)
  df = data.frame(x = rnorm(1000), y = 1)

  # using geom_[half]eyeh here to test deprecated aliases
  p = ggplot(df, aes(x = x, y = y))
  expect_warning(vdiffr::expect_doppelganger("one-parameter horizontal eye",
    p + geom_eyeh(n = 20)
  ), "Deprecated")
  expect_warning(vdiffr::expect_doppelganger("one-parameter horizontal half-eye",
    p + geom_halfeyeh(n = 20)
  ), "Deprecated")

  # using geom_eye here to test deprecated alias (there never was a geom_halfeye())
  p = ggplot(df, aes(x = y, y = x))
  expect_warning(vdiffr::expect_doppelganger("one-parameter vertical eye",
    p + geom_eye(n = 20)
  ), "Deprecated")
  vdiffr::expect_doppelganger("one-parameter vertical halfeye", p + stat_halfeye(n = 20))

  p = ggplot(df, aes(x = x, y = y))
  expect_warning(vdiffr::expect_doppelganger("one-parameter horizontal eye (mode_hdi)",
    p + stat_eyeh(point_interval = mode_hdi, n = 20)
  ), "Deprecated")

})


test_that("two-parameter eye plots work", {
  skip_if_no_vdiffr()
  skip_if_sensitive_to_density()


  set.seed(123)
  df = data.frame(x = rnorm(1000), y = "a", y_int = 1) %>%
    rbind(data.frame(x = rnorm(1000, 1), y = "b", y_int = 2))

  p = ggplot(df, aes(x = x, y = y))
  expect_warning(vdiffr::expect_doppelganger("two-parameter (factor) horizontal half-eye",
    p + stat_halfeyeh(scale = 0.5, n = 20)
  ), "Deprecated")
  vdiffr::expect_doppelganger("two-parameter (factor) horizontal eye (fill)", p + stat_eye(aes(fill = y), scale = 0.5, n = 20))

})
