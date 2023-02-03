# Tests for add_draws
#
# Author: mjskay
###############################################################################

library(dplyr)
library(tidyr)
library(magrittr)




# data
mtcars_tbl = mtcars %>%
  set_rownames(seq_len(nrow(.))) %>%
  as_tibble()


test_that("add_draws works on a simple example", {
  y = array(1:24/6, dim = c(6, 4))
  df = data.frame(x = 1:4L)

  ref = data.frame(
      x = rep(1:4L, each = 6),
      .row = rep(1:4L, each = 6),
      .draw = rep(1:6L, 4),
      .value = 1:24/6
    ) %>%
    group_by(x, .row)

  expect_equal(add_draws(df, y), ref)

  y2 = y
  dim(y2) = c(2,3,4)
  expect_error(add_draws(df, y2), "`draws` must have exactly two dimensions. It has 3")
})

test_that("add_draws works on fit from a simple rstanarm model", {
  skip_if_not_installed("rstanarm")
  m_hp_wt = readRDS(test_path("../models/models.rstanarm.m_hp_wt.rds"))

  fits_matrix = rstanarm::posterior_linpred(m_hp_wt, newdata = mtcars_tbl)

  fits = fits_matrix %>%
    as.data.frame() %>%
    mutate(.draw = seq_len(n())) %>%
    gather(.row, .value, -.draw) %>%
    as_tibble()

  ref = mtcars_tbl %>%
    mutate(.row = rownames(.)) %>%
    inner_join(fits, by = ".row", multiple = "all") %>%
    mutate(.row = as.integer(.row)) %>%
    group_by(mpg, cyl, disp, hp, drat, wt, qsec, vs, am, gear, carb, .row)

  expect_equal(add_draws(mtcars, fits_matrix), ref)
  expect_equal(add_draws(mtcars_tbl, fits_matrix), ref)
})
