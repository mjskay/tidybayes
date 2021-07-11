# Tests for [add_]predicted_rvars
#
# Author: mjskay
###############################################################################

suppressWarnings(suppressMessages({
  library(dplyr)
  library(tidyr)
  library(magrittr)
  library(posterior)
}))




# data
mtcars_tbl = mtcars %>%
  set_rownames(seq_len(nrow(.))) %>%
  as_tibble()

# for reliable testing, need to use only a single core (otherwise random
# numbers do not seem to always be reproducible on brms)
options(mc.cores = 1)


test_that("[add_]predicted_rvars and basic arguments works on a simple rstanarm model", {
  skip_if_not_installed("rstanarm")
  m_hp_wt = readRDS(test_path("../models/models.rstanarm.m_hp_wt.rds"))

  ref = mtcars_tbl %>%
    mutate(.prediction = rvar(rstantools::posterior_predict(m_hp_wt, mtcars_tbl, draws = 100, seed = 123)))

  expect_equal(predicted_rvars(m_hp_wt, mtcars_tbl, ndraws = 100, seed = 123), ref)
  expect_equal(add_predicted_rvars(mtcars_tbl, m_hp_wt, ndraws = 100, seed = 123), ref)

  #predicted_rvars.default should work fine here so long as we don't subset
  ref_all = mtcars_tbl %>%
    mutate(.prediction = rvar(rstantools::posterior_predict(m_hp_wt, mtcars_tbl, seed = 123)))
  expect_equal(predicted_rvars.default(m_hp_wt, mtcars_tbl, seed = 123), ref_all)
})


test_that("[add_]predicted_rvars and basic arguments works on an rstanarm model with random effects", {
  skip_if_not_installed("rstanarm")
  m_cyl = readRDS(test_path("../models/models.rstanarm.m_cyl.rds"))

  ref = mtcars_tbl %>%
    mutate(.prediction = rvar(rstantools::posterior_predict(m_cyl, mtcars_tbl, draws = 100, seed = 123)))

  expect_equal(predicted_rvars(m_cyl, mtcars_tbl, ndraws = 100, seed = 123), ref)
  expect_equal(add_predicted_rvars(mtcars_tbl, m_cyl, ndraws = 100, seed = 123), ref)
})


test_that("[add_]predicted_rvars works on a simple brms model", {
  skip_if_not_installed("brms")
  m_hp = readRDS(test_path("../models/models.brms.m_hp.rds"))

  set.seed(123)
  ref = mtcars_tbl %>%
    mutate(.prediction = rvar(rstantools::posterior_predict(m_hp, mtcars_tbl, nsamples = 100)))

  expect_equal(predicted_rvars(m_hp, mtcars_tbl, ndraws = 100, seed = 123), ref)
  expect_equal(add_predicted_rvars(mtcars_tbl, m_hp, ndraws = 100, seed = 123), ref)
})

test_that("[add_]predicted_rvars works on brms models with categorical outcomes", {
  skip_if_not_installed("brms")
  m_cyl_mpg = readRDS(test_path("../models/models.brms.m_cyl_mpg.rds"))

  set.seed(1234)
  ref = mtcars_tbl %>%
    mutate(.prediction = rvar(rstantools::posterior_predict(m_cyl_mpg, mtcars_tbl, nsamples = 100)))

  expect_equal(predicted_rvars(m_cyl_mpg, mtcars_tbl, seed = 1234, ndraws = 100), ref)
  expect_equal(add_predicted_rvars(mtcars_tbl, m_cyl_mpg, seed = 1234, ndraws = 100), ref)
})

test_that("[add_]predicted_rvars works on brms models with dirichlet responses", {
  skip_if_not_installed("brms")
  m_dirich = readRDS(test_path("../models/models.brms.m_dirich.rds"))

  set.seed(1234)
  grid = tibble(x = c("A", "B"))
  ref = grid %>%
    mutate(.prediction = rvar(rstantools::posterior_predict(m_dirich, grid, nsamples = 100)))

  expect_equal(predicted_rvars(m_dirich, grid, seed = 1234, ndraws = 100), ref)
})

test_that("[add_]predicted_rvars works on brms models with multinomial responses", {
  skip_if_not_installed("brms")
  m_multinom = readRDS(test_path("../models/models.brms.m_multinom.rds"))

  set.seed(1234)
  # use a low number for total so there are some 0s
  grid = tibble(total = c(10, 20))
  ref = grid %>%
    mutate(.prediction = rvar(rstantools::posterior_predict(m_multinom, grid, nsamples = 10)))

  expect_equal(predicted_rvars(m_multinom, grid, seed = 1234, ndraws = 10), ref)

  # column transformation
  column_ref = ref %>%
    mutate(.row = 1:n()) %>%
    group_by(across(-.prediction)) %>%
    summarise(col_pred = colnames(.prediction), .prediction = t(.prediction), .groups = "drop") %>%
    arrange(col_pred, .row)
  dim(column_ref$.prediction) = length(column_ref$.prediction)
  attr(draws_of(column_ref$.prediction), "levels") = c("a","b","c")

  expect_equal(predicted_rvars(m_multinom, grid, seed = 1234, ndraws = 10, columns_to = "col_pred"), column_ref)
})

test_that("[add_]predicted_rvars throws an error when nsamples is called instead of ndraws in brms", {
  skip_if_not_installed("brms")
  m_hp = readRDS(test_path("../models/models.brms.m_hp.rds"))

  expect_error(
    m_hp %>% predicted_rvars(mtcars_tbl, nsamples = 100),
    "`nsamples.*.`ndraws`.*.See the documentation for additional details."
  )
  expect_error(
    mtcars_tbl %>% add_predicted_rvars(m_hp, nsamples = 100),
    "`nsamples.*.`ndraws`.*.See the documentation for additional details."
  )
})

test_that("[add_]predicted_rvars throws an error when draws is called instead of ndraws in rstanarm", {
  skip_if_not_installed("rstanarm")
  m_hp_wt = readRDS(test_path("../models/models.rstanarm.m_hp_wt.rds"))

  expect_error(
    m_hp_wt %>% predicted_rvars(mtcars_tbl, draws = 100),
    "`draws.*.`ndraws`.*.See the documentation for additional details."
  )
  expect_error(
    mtcars_tbl %>% add_predicted_rvars(m_hp_wt, draws = 100),
    "`draws.*.`ndraws`.*.See the documentation for additional details."
  )
})

test_that("[add_]predicted_rvars throws an error when re.form is called instead of re_formula in rstanarm", {
  skip_if_not_installed("rstanarm")
  m_hp_wt = readRDS(test_path("../models/models.rstanarm.m_hp_wt.rds"))

  expect_error(
    m_hp_wt %>% predicted_rvars(mtcars_tbl, re.form = NULL),
    "`re.form.*.`re_formula`.*.See the documentation for additional details."
  )
  expect_error(
    mtcars_tbl %>% add_predicted_rvars(m_hp_wt, re.form = NULL),
    "`re.form.*.`re_formula`.*.See the documentation for additional details."
  )
})
