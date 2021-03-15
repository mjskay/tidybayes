# Tests for gather_rvars
#
# Author: mjskay
###############################################################################

library(dplyr)
library(posterior)

context("gather_rvars")


#set up datasets
data(RankCorr, package = "ggdist")

# subset of RankCorr (for speed)
RankCorr_s = as_draws_rvars(RankCorr[[1]][1:10,])

# version of RankCorr with i index labeled
i_labels = c("a", "b", "c")
RankCorr_i = recover_types(RankCorr_s, list(i = factor(i_labels)))

# version of RankCorr with i and j dimensions labeled
i_labels = c("a", "b", "c")
j_labels = c("A", "B", "C", "D")
RankCorr_ij = recover_types(RankCorr_s, list(i = factor(i_labels), j = factor(j_labels)))


# tests for gather_rvars ===================================================

test_that("gather_rvars works on a simple variable with no dimensions", {
  ref = tibble(
    .variable = "typical_r",
    .value = RankCorr_s$typical_r
  )

  expect_equivalent(gather_rvars(RankCorr_s, typical_r), ref)
})

test_that("gather_rvars works on two variables with no dimensions and multiple chains", {
  data(line, package = "coda")
  line = as_draws_rvars(line)

  ref = tibble(
    .variable = c("alpha", "beta"),
    .value = c(line$alpha, line$beta)
  )

  expect_equal(gather_rvars(line, alpha, beta), ref)
  expect_equal(gather_rvars(line, c(alpha, beta)), ref)
  expect_equal(gather_rvars(line, alpha[], beta[]), ref)
})

test_that("gather_rvars allows extraction of two variables simultaneously with a wide index", {
  ref = tibble(
    .variable = c("tau", "u_tau"),
    .value = rbind(t(RankCorr_i$tau), t(RankCorr_i$u_tau))
  )

  expect_equal(gather_rvars(RankCorr_s, c(tau, u_tau)[]), ref)
})

test_that("gather_rvars correctly extracts multiple variables simultaneously", {
  ref = tibble(
    i = rep(factor(i_labels), 2),
    .variable = rep(c("tau", "u_tau"), each = 3),
    .value = c(RankCorr_i$tau, RankCorr_i$u_tau)
  )

  expect_equal(gather_rvars(RankCorr_i, c(tau, u_tau)[i]), ref)
  expect_equal(gather_rvars(RankCorr_i, cbind(tau, u_tau)[i]), ref)
  expect_equal(gather_rvars(RankCorr_i, tau[i], u_tau[i]), ref)
  expect_equal(gather_rvars(RankCorr_i, cbind(tau)[i]), ref[1:3,])
})

test_that("gather_rvars multispec syntax combines results correctly", {
  i_b = rep(1:3, each = 4)
  i_tau = 1:3
  v_b = rep(1:4, 3)

  ref = tibble(
    .variable = c(rep("b", 3*4), rep("tau", 3), "typical_r"),
    .value = c(RankCorr_ij$b[cbind(i_b,v_b)], RankCorr_ij$tau[i_tau], RankCorr_ij$typical_r),
    i = factor(c(i_b, i_tau, NA), labels = i_labels),
    v = c(v_b, NA, NA, NA, NA)
  )

  expect_equal(gather_rvars(RankCorr_ij, typical_r, tau[i], b[i, v]) %>% arrange(.variable,i,v), ref)
})
