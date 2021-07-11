# Tests for [add_]residual_draws
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


test_that("[add_]residual_draws throws error on unsupported models", {
  expect_error(residual_draws(list()), "Models of type \"list\" are not currently supported by `residual_draws`")
})

test_that("[add_]residual_draws works on a simple brms model", {
  skip_if_not_installed("brms")
  m_hp = readRDS(test_path("../models/models.brms.m_hp.rds"))

  make_ref = function(nsamples = NULL) {
    set.seed(1234)
    resids = residuals(m_hp, mtcars_tbl, summary = FALSE, nsamples = nsamples) %>%
      as.data.frame() %>%
      set_names(seq_len(ncol(.))) %>%
      mutate(
        .chain = NA_integer_,
        .iteration = NA_integer_,
        .draw = seq_len(n())
      ) %>%
      gather(.row, .residual, -.chain, -.iteration, -.draw) %>%
      as_tibble()

    mtcars_tbl %>%
      mutate(.row = rownames(.)) %>%
      inner_join(resids, by = ".row") %>%
      mutate(.row = as.integer(.row)) %>%
      group_by(mpg, cyl, disp, hp, drat, wt, qsec, vs, am, gear, carb, .row)
  }
  ref = make_ref()

  expect_equal(residual_draws(m_hp, mtcars_tbl), ref)
  expect_equal(add_residual_draws(mtcars_tbl, m_hp), ref)

  # subsetting to test `ndraws`
  set.seed(1234)
  filtered_ref = make_ref(nsamples = 10)
  expect_equal(residual_draws(m_hp, mtcars_tbl, ndraws = 10, seed = 1234), filtered_ref)
  expect_equal(add_residual_draws(mtcars_tbl, m_hp, ndraws = 10, seed = 1234), filtered_ref)
  expect_warning(
    expect_equal(residual_draws(m_hp, mtcars_tbl, n = 10, seed = 1234), filtered_ref),
    "`n`.*deprecated.*`ndraws`"
  )
  expect_warning(
    expect_equal(add_residual_draws(mtcars_tbl, m_hp, n = 10, seed = 1234), filtered_ref),
    "`n`.*deprecated.*`ndraws`"
  )
})
