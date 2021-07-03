# Tests for dots geoms and stats
#
# Author: mjskay
###############################################################################

library(dplyr)

context("geom_dotsinterval")

test_that("vanilla dots geoms and stats work", {
  skip_if_no_vdiffr()


  set.seed(1234)
  p = tribble(
    ~dist,  ~x,
    "norm", rnorm(20),
    "t",    rt(20, 3)
  ) %>%
    unnest(x) %>%
    ggplot()

  expect_warning(vdiffr::expect_doppelganger("vanilla geom_dotsh",
    p + geom_dotsh(aes(y = dist, x = x))
  ), "Deprecated")

  expect_warning(vdiffr::expect_doppelganger("stat_dotsh with a group with 1 dot",
    p + stat_dotsh(aes(y = dist, x = x, color = x > 2))
  ), "Deprecated")
})
