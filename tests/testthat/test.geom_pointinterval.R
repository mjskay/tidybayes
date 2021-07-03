# Tests for interval geoms
#
# Author: mjskay
###############################################################################

library(dplyr)
library(tidyr)

context("geom_pointinterval")

# use a subset of RankCorr so tests are faster
data(RankCorr, package = "ggdist")
RankCorr_u_tau = RankCorr %>%
  spread_draws(u_tau[i]) %>%
  filter(i %in% 1:3, .iteration %in% 1:50)

test_that("horizontal grouped pointintervals work", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("svglite")

  expect_warning(vdiffr::expect_doppelganger("grouped pointintervals (h)",
    RankCorr_u_tau %>%
      median_hdci(.width = c(.66, .95)) %>%
      ggplot(aes(y = i, x = u_tau)) +
      geom_pointintervalh(show.legend = TRUE) +
      theme_tidybayes()
  ))

  expect_warning(vdiffr::expect_doppelganger("grouped pointintervals (h, stat)",
    RankCorr_u_tau %>%
      ggplot(aes(y = i, x = u_tau)) +
      stat_pointintervalh(.width = c(.66, .95))
  ), "Deprecated")

})
