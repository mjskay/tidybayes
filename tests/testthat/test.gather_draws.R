# Tests for gather_draws
#
# Author: mjskay
###############################################################################

library(dplyr)
library(tidyr)


test_that("regular expressions for parameter names work on non-indexed parameters", {
  data(RankCorr, package = "ggdist")

  ref = RankCorr %>% spread_draws(typical_r) %>% gather_variables()

  expect_equal(gather_draws(RankCorr, `typical..`, regex = TRUE), ref)
})

test_that("regular expressions for parameter names work on indexed parameters", {
  data(RankCorr, package = "ggdist")

  ref = RankCorr %>% spread_draws(c(tau, u_tau)[i]) %>% gather_variables()

  expect_equal(gather_draws(RankCorr, `.*tau`[i], regex = TRUE), ref)
})

test_that("gather_draws works on a combination of 0 and 1-dimensional values (with correct groups)", {
  data(RankCorr, package = "ggdist")

  ref = RankCorr %>%
    spread_draws(tau[i], typical_r) %>%
    gather(".variable", ".value", -one_of(".chain", ".iteration", ".draw", "i")) %>%
    ungroup() %>%
    filter(.variable != "typical_r" | (.variable == "typical_r" & i == 1))
  ref[ref$.variable == "typical_r", "i"] = NA
  ref = group_by(ref, i, .variable, .drop = FALSE)

  result = gather_draws(RankCorr, tau[i], typical_r)

  # grouped tibble equivalence is too persnickety now, so test equivalence + equal groups
  expect_equivalent(result, ref)
  expect_equal(group_vars(result), group_vars(ref))
})

test_that("draw_indices works", {
  df = data.frame(
    .chain = rep(1:4, each = 4),
    .iteration = rep(1:4, 4),
    .draw = 1:16,
    .warmup = rep(c(TRUE, TRUE, FALSE, FALSE), 4),
    `x[1]` = 2:17,
    `x[2]` = 3:18,
    check.names = FALSE
  )

  ref = tibble(
    i = rep(1:2, each = 16),
    .chain = rep(1:4, each = 4, times = 2),
    .iteration = rep(1:4, 8),
    .draw = rep(1:16, 2),
    .warmup = rep(c(TRUE, TRUE, FALSE, FALSE), 8),
    .variable = "x",
    .value = c(2:17, 3:18)
  ) %>%
    group_by(i, .variable)

  result = gather_draws(df, x[i], draw_indices = c(".chain", ".iteration", ".draw", ".warmup"))

  expect_equivalent(result, ref)
  expect_equal(group_vars(result), group_vars(ref))
})
