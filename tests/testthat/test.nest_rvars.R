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

test_that("spread_rvars correctly rejects missing variables", {
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
