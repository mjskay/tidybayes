# Tests for [add_]linpred_rvars
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


test_that("[add_]linpred_rvars works on a simple rstanarm model", {
  skip_if_not_installed("rstanarm")
  m_hp_wt = readRDS(test_path("../models/models.rstanarm.m_hp_wt.rds"))

  ref = mtcars_tbl %>%
    mutate(.linpred = rvar(rstantools::posterior_linpred(m_hp_wt, newdata = mtcars_tbl)))

  expect_equal(linpred_rvars(m_hp_wt, mtcars_tbl), ref)
  expect_equal(add_linpred_rvars(mtcars_tbl, m_hp_wt, linpred = "foo"), rename(ref, foo = .linpred))

  #linpred_rvars.default should work fine here
  expect_equal(linpred_rvars.default(m_hp_wt, mtcars_tbl), ref)

  #subsetting to test the `ndraws` argument
  set.seed(1234)
  draw_subset = sample.int(ndraws(ref$.linpred), 10)
  filtered_ref = ref
  draws_of(filtered_ref$.linpred) = draws_of(filtered_ref$.linpred)[draw_subset,]
  dimnames(draws_of(filtered_ref$.linpred))[[1]] = 1:10

  expect_equal(add_linpred_rvars(mtcars_tbl, m_hp_wt, ndraws = 10, seed = 1234), filtered_ref)
})


test_that("[add_]linpred_rvars works on brms models with dpar", {
  skip_if_not_installed("brms")
  m_hp_sigma = readRDS(test_path("../models/models.brms.m_hp_sigma.rds"))

  ref = mtcars_tbl %>%
    mutate(
      .linpred = rvar(rstantools::posterior_linpred(m_hp_sigma, newdata = mtcars_tbl)),
      mu = rvar(rstantools::posterior_linpred(m_hp_sigma, newdata = mtcars_tbl, dpar = "mu")),
      sigma = rvar(rstantools::posterior_linpred(m_hp_sigma, newdata = mtcars_tbl, dpar = "sigma"))
    )

  expect_equal(linpred_rvars(m_hp_sigma, mtcars_tbl, dpar = TRUE), ref)
  expect_equal(add_linpred_rvars(mtcars_tbl, m_hp_sigma, dpar = TRUE), ref)
  expect_equal(add_linpred_rvars(mtcars_tbl, m_hp_sigma, dpar = "sigma"), select(ref, -mu))
  expect_equal(add_linpred_rvars(mtcars_tbl, m_hp_sigma, dpar = "mu"), select(ref, -sigma))
  expect_equal(add_linpred_rvars(mtcars_tbl, m_hp_sigma, dpar = FALSE), select(ref, -sigma, -mu))
  expect_equal(add_linpred_rvars(mtcars_tbl, m_hp_sigma, dpar = NULL), select(ref, -sigma, -mu))
  expect_equal(add_linpred_rvars(mtcars_tbl, m_hp_sigma, dpar = list("mu", "sigma", s1 = "sigma")), mutate(ref, s1 = sigma))


  #subsetting to test the `ndraws` argument
  set.seed(1234)
  draw_subset = sample.int(ndraws(ref$.linpred), 10)
  filtered_ref = ref
  draws_of(filtered_ref$.linpred) = draws_of(filtered_ref$.linpred)[draw_subset,]
  dimnames(draws_of(filtered_ref$.linpred))[[1]] = 1:10
  draws_of(filtered_ref$mu) = draws_of(filtered_ref$mu)[draw_subset,]
  dimnames(draws_of(filtered_ref$mu))[[1]] = 1:10
  draws_of(filtered_ref$sigma) = draws_of(filtered_ref$sigma)[draw_subset,]
  dimnames(draws_of(filtered_ref$sigma))[[1]] = 1:10

  expect_equal(add_linpred_rvars(mtcars_tbl, m_hp_sigma, ndraws = 10, seed = 1234, dpar = TRUE), filtered_ref)
})


test_that("[add_]linpred_rvars works on brms models with ordinal outcomes", {
  skip_if_not_installed("brms")
  m_cyl_mpg = readRDS(test_path("../models/models.brms.m_cyl_mpg.rds"))

  ref = mtcars_tbl %>%
    mutate(.linpred = rvar(rstantools::posterior_linpred(m_cyl_mpg, newdata = mtcars_tbl)))

  expect_equal(linpred_rvars(m_cyl_mpg, mtcars_tbl), ref)
  expect_equal(add_linpred_rvars(mtcars_tbl, m_cyl_mpg), ref)
})


test_that("[add_]linpred_rvars works on brms models with dirichlet outcomes", {
  skip_if_not_installed("brms")
  m_dirich = readRDS(test_path("../models/models.brms.m_dirich.rds"))

  grid = tibble(x = c("A", "B"))
  ref = grid %>%
    mutate(.linpred = rvar(rstantools::posterior_linpred(m_dirich, newdata = grid)))

  expect_equal(linpred_rvars(m_dirich, grid), ref)

  # column transformation
  column_ref = ref %>%
    mutate(.row = 1:n()) %>%
    group_by(across(-.linpred)) %>%
    summarise(g_pred = colnames(.linpred) %||% 1:ncol(.linpred), .linpred = t(.linpred), .groups = "drop") %>%
    arrange(g_pred, .row)
  dim(column_ref$.linpred) = length(column_ref$.linpred)
  expect_equal(add_linpred_rvars(grid, m_dirich, columns_to = "g_pred"), column_ref)
})


test_that("[add_]linpred_rvars throws an error when nsamples is called instead of ndraws in brms", {
  skip_if_not_installed("brms")
  m_hp = readRDS(test_path("../models/models.brms.m_hp.rds"))

  expect_error(
    m_hp %>% linpred_rvars(mtcars_tbl, nsamples = 10),
    "`nsamples.*.`ndraws`.*.See the documentation for additional details."
  )
  expect_error(
    mtcars_tbl %>% add_linpred_rvars(m_hp, nsamples = 10),
    "`nsamples.*.`ndraws`.*.See the documentation for additional details."
  )
})


test_that("[add_]linpred_rvars throws an error when re.form is called instead of re_formula in rstanarm", {
  skip_if_not_installed("rstanarm")
  m_hp_wt = readRDS(test_path("../models/models.rstanarm.m_hp_wt.rds"))

  expect_error(
    m_hp_wt %>% linpred_rvars(mtcars_tbl, re.form = NULL),
    "`re.form.*.`re_formula`.*.See the documentation for additional details."
  )
  expect_error(
    mtcars_tbl %>% add_linpred_rvars(m_hp_wt, re.form = NULL),
    "`re.form.*.`re_formula`.*.See the documentation for additional details."
  )
})
