# Tests for spread_rvars
#
# Author: mjskay
###############################################################################

library(dplyr)
library(posterior)




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


# tests for spread_rvars ===================================================

test_that("spread_rvars correctly rejects missing variables", {
  data("RankCorr", package = "ggdist")

  expect_error(spread_rvars(RankCorr, c(a, b)),
    "The variable .* was not found in the model")
  expect_error(spread_rvars(RankCorr, a[b]),
    "The variable .* was not found in the model")
  expect_error(spread_rvars(RankCorr, c(a, x)[b]),
    "The variable .* was not found in the model")
})


test_that("spread_rvars works on a simple variable with no dimensions", {
  ref = tibble(
    typical_r = RankCorr_s$typical_r
  )

  expect_equal(spread_rvars(RankCorr_s, typical_r), ref)

  # subsetting
  set.seed(1234)
  RankCorr_draws = as_draws(RankCorr_s)
  RankCorr_subsample = RankCorr_draws %>%
    weight_draws(rep(1, ndraws(RankCorr_draws))) %>%
    resample_draws(ndraws = 5)
  subsample_ref = tibble(
    typical_r = RankCorr_subsample$typical_r
  )
  expect_equal(spread_rvars(RankCorr_s, typical_r, ndraws = 5, seed = 1234), subsample_ref)
})


test_that("spread_rvars works on two variables with no dimensions and multiple chains", {
  data(line, package = "coda")
  line = as_draws_rvars(line)

  ref = tibble(
    alpha = line$alpha,
    beta = line$beta
  )

  expect_equal(spread_rvars(line, alpha, beta), ref)
  expect_equal(spread_rvars(line, c(alpha, beta)), ref)
  expect_equal(spread_rvars(line, alpha[], beta[]), ref)
})


test_that("spread_rvars works on a variable with one unnamed index", {
  ref = tibble(
    i = 1:3,
    tau = RankCorr_s$tau
  )

  expect_equal(spread_rvars(RankCorr_s, tau[i]) %>% arrange(i), ref)
})

test_that("spread_rvars works on a variable with one named index", {
  ref = tibble(
    i = factor(c("a","b","c")),
    tau = RankCorr_s$tau
  )

  expect_equal(spread_rvars(RankCorr_i, tau[i]) %>% arrange(i), ref)
})

test_that("spread_rvars works on a variable with one index left wide", {
  ref = tibble(
    tau = t(RankCorr_s$tau)
  )

  expect_equal(spread_rvars(RankCorr_s, tau[]), ref)
})


test_that("spread_rvars works on a variable with one named wide index", {
  tau = t(RankCorr_s$tau)
  dimnames(tau) = list(NULL, c("a","b","c"))

  ref = tibble(
    tau = tau
  )

  RankCorr_i_abc = RankCorr_i
  names(RankCorr_i_abc$tau) = c("a","b","c")

  expect_equal(spread_rvars(RankCorr_i_abc, tau[]), ref)
})


test_that("spread_rvars works on a variable with two named dimensions", {
  i = rep(1:3, 4)
  j = rep(1:4, each = 3)
  ref = tibble(
    i = factor(i_labels[i]),
    j = factor(j_labels[j]),
    b = RankCorr_ij$b[cbind(i,j)]
  )

  expect_equal(spread_rvars(RankCorr_ij, b[i, j]) %>% arrange(j, i), ref)
})


test_that("spread_rvars works on a variable with one named index and one wide index", {
  ref = tibble(
    i = factor(i_labels),
    b = RankCorr_i$b
  )

  # grouping attributes are too finicky on this one for an exact comparison
  expect_equivalent(spread_rvars(RankCorr_i, b[i, ]) %>% arrange(i), ref)
})

test_that("spread_rvars allows extraction of two variables simultaneously with a wide index", {
  ref = tibble(
    tau = t(RankCorr_i$tau),
    u_tau = t(RankCorr_i$u_tau)
  )

  expect_equal(spread_rvars(RankCorr_s, c(tau, u_tau)[]), ref)
})

test_that("spread_rvars correctly extracts multiple variables simultaneously", {
  ref = tibble(
    i = factor(i_labels),
    tau = RankCorr_i$tau,
    u_tau = RankCorr_i$u_tau
  )

  expect_equal(spread_rvars(RankCorr_i, c(tau, u_tau)[i]), ref)
  expect_equal(spread_rvars(RankCorr_i, cbind(tau, u_tau)[i]), ref)
  expect_equal(spread_rvars(RankCorr_i, cbind(tau)[i]), ref[-3])
})

test_that("spread_rvars correctly extracts multiple variables simultaneously when those variables have no dimensions", {
  RankCorr_t = RankCorr_s
  RankCorr_t$tr2 = RankCorr_t$tau[[1]]

  ref = tibble(
    typical_r = RankCorr_t$typical_r,
    tr2 = RankCorr_t$tr2
  )

  expect_equal(spread_rvars(RankCorr_t, c(typical_r, tr2)), ref)
})

test_that("spread_rvars multispec syntax joins results correctly", {
  i_int = rep(1:3, each = 4)
  v = rep(1:4, 3)

  ref = tibble(
    typical_r = RankCorr_ij$typical_r,
    i = factor(i_int, labels = i_labels),
    tau = RankCorr_ij$tau[i_int],
    v = v,
    b = RankCorr_ij$b[cbind(i_int,v)]
  )

  expect_equal(spread_rvars(RankCorr_ij, typical_r, tau[i], b[i, v]) %>% arrange(i,v), ref)
})
