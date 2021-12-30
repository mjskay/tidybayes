# Tests for nest_rvars
#
# Author: mjskay
###############################################################################

library(dplyr)
library(posterior)




#set up datasets
data(RankCorr, package = "ggdist")

# subset of RankCorr (for speed)
RankCorr_s = as_draws_rvars(RankCorr[[1]][1:10,])


# tests for nest_rvars ===================================================

test_that("nest_rvars works", {
  rvar_df = RankCorr_s %>%
    spread_rvars(b[i,], tau[i])

  draws_df = RankCorr_s %>%
    spread_draws(b[i,..], tau[i]) %>%
    rename(

    ) %>%
    select(i,
      `b[1,1]` = b.1,
      `b[1,2]` = b.2,
      `b[1,3]` = b.3,
      `b[1,4]` = b.4,
      tau, .chain, .iteration, .draw
    )

  expect_equal(unnest_rvars(rvar_df), draws_df)
  expect_equal(nest_rvars(draws_df), group_by(rvar_df, i))
})


# .chain / .iteration columns work -------------------------------------

test_that("chains and iterations work", {
  rvar_df = tibble(
      i = 1:2,
      x = rvar(array(1:16/4, dim = c(4,2,2)), nchains = 2)
    ) %>%
    group_by(i)
  draws_df = tibble(
      i = rep(1:2L, each = 4),
      `x[1,1]` = 1:8/4,
      `x[1,2]` = 9:16/4,
      .chain = rep(c(1L,2L,1L,2L), each = 2),
      .iteration = rep(1:2L, 4),
      .draw = rep(1:4L, 2)
    ) %>%
    group_by(i)

  expect_equal(unnest_rvars(rvar_df), draws_df)
  expect_equal(nest_rvars(draws_df), rvar_df)

  # missing .iteration
  rvar_df_one_chain = rvar_df
  rvar_df_one_chain$x = rvar(draws_of(rvar_df$x), nchains = 1)
  draws_df_no_iteration = draws_df
  draws_df_no_iteration$.iteration = NA
  expect_equal(nest_rvars(draws_df_no_iteration), rvar_df_one_chain)
  draws_df_no_iteration$.iteration = NULL
  expect_equal(nest_rvars(draws_df_no_iteration), rvar_df_one_chain)

  # missing .chain
  draws_df_no_chain = draws_df
  draws_df_no_chain$.chain = NA
  expect_equal(nest_rvars(draws_df_no_chain), rvar_df_one_chain)
  draws_df_no_chain$.chain = NULL
  expect_equal(nest_rvars(draws_df_no_chain), rvar_df_one_chain)
})


test_that("missing / NA .chain and .iteration columns work", {
  rvar_df = RankCorr_s %>%
    spread_rvars(b[i,], tau[i])

  draws_df = RankCorr_s %>%
    spread_draws(b[i,..], tau[i]) %>%
    rename(

    ) %>%
    select(i,
      `b[1,1]` = b.1,
      `b[1,2]` = b.2,
      `b[1,3]` = b.3,
      `b[1,4]` = b.4,
      tau, .chain, .iteration, .draw
    )

  expect_equal(unnest_rvars(rvar_df), draws_df)
  expect_equal(nest_rvars(draws_df), group_by(rvar_df, i))
})
