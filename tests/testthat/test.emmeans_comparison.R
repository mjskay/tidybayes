# Tests for emmeans_comparison
#
# Author: mjskay
###############################################################################

context("emmeans_comparison")


test_that("basic conversion of emmeans contrasts works", {

  pairwise_ref = list(
    `a - b` = quote(1 * a + -1 * b +  0 * c),
    `a - c` = quote(1 * a +  0 * b + -1 * c),
    `b - c` = quote(0 * a +  1 * b + -1 * c)
  )
  expect_equal(emmeans_comparison(emmeans:::pairwise.emmc)(c("a","b","c")), pairwise_ref)
  expect_equal(emmeans_comparison("pairwise")(c("a","b","c")), pairwise_ref)

  mean_chg_ref = list(
    `a|b` = quote(  -1 * a +  0.5 * b + 0.5 * c),
    `b|c` = quote(-0.5 * a + -0.5 * b +   1 * c)
  )
  expect_equal(emmeans_comparison(emmeans:::mean_chg.emmc)(c("a","b","c")), mean_chg_ref)
  expect_equal(emmeans_comparison("mean_chg")(c("a","b","c")), mean_chg_ref)
})
