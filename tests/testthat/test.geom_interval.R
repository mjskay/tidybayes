# Tests for interval geoms
#
# Author: mjskay
###############################################################################

library(dplyr)
library(tidyr)

context("geom_interval")

# use a subset of RankCorr so tests are faster
data(RankCorr, package = "ggdist")
RankCorr_s = RankCorr[[1]][1:100,]
RankCorr_u_tau = RankCorr_s %>%
  spread_draws(u_tau[i]) %>%
  filter(i %in% 1:3)

test_that("horizontal grouped intervals work", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("svglite")

  expect_warning(vdiffr::expect_doppelganger("grouped intervals (h)",
    RankCorr_u_tau %>%
      mean_qi(.width = c(.5, .75, .90)) %>%
      ggplot(aes(y = i, x = u_tau)) +
      geom_intervalh() +
      scale_color_brewer()
  ), "Deprecated")

  expect_warning(vdiffr::expect_doppelganger("grouped intervals (h, stat)",
    RankCorr_u_tau %>%
      ggplot(aes(y = i, x = u_tau)) +
      stat_intervalh(.width = c(.5, .75, .90)) +
      scale_color_brewer()
  ), "Deprecated")
})
