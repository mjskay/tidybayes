# Tests for sample_draws
#
# Author: mjskay
###############################################################################

library(dplyr)
library(tidyr)


# use a subset of RankCorr so tests are faster
data(RankCorr, package = "ggdist")
RankCorr_s = RankCorr[[1]][1:100,]
RankCorr_u_tau = RankCorr_s %>%
  spread_draws(u_tau[i]) %>%
  filter(i %in% 1:3)

test_that("sample_draws works", {
  set.seed(1234)
  draws = sample.int(100, 2)
  ref = filter(RankCorr_u_tau, .draw %in% draws)

  expect_equal(sample_draws(RankCorr_u_tau, ndraws = 2, seed = 1234), ref)
})
