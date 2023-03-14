# Tests for summarise_draws.grouped_df
#
# Author: mjskay
###############################################################################

library(posterior)
library(dplyr)




test_that("summarise_draws.grouped_df works", {
  d = subset_draws(example_draws(), variable = c("theta[1]", "theta[2]", "theta[3]"))

  ref = summarise_draws(d) %>%
    mutate(variable = "theta") %>%
    add_column(i = 1:3, .before = 1) %>%
    group_by(i)
  attr(ref, "num_args") = NULL

  expect_equal(d %>% spread_draws(theta[i]) %>% summarise_draws(), ref)
})
