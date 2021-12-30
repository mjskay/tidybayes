# Tests for [add_]epred_rvars
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


test_that("[add_]epred_rvars works on a simple rstanarm model", {
  skip_if_not_installed("rstanarm")
  m_hp_wt = readRDS(test_path("../models/models.rstanarm.m_hp_wt.rds"))

  ref = mtcars_tbl %>%
    mutate(.epred = rvar(rstantools::posterior_epred(m_hp_wt, newdata = mtcars_tbl)))

  expect_equal(epred_rvars(m_hp_wt, mtcars_tbl), ref)
  expect_equal(add_epred_rvars(mtcars_tbl, m_hp_wt), ref)
  expect_equal(add_epred_rvars(mtcars_tbl, m_hp_wt, value = "foo"), rename(ref, foo = .epred))

  #epred_rvars.default should work fine here
  expect_equal(epred_rvars.default(m_hp_wt, mtcars_tbl), ref)

  #subsetting to test the `ndraws` argument
  set.seed(1234)
  draw_subset = sample.int(ndraws(ref$.epred), 10)
  filtered_ref = ref
  draws_of(filtered_ref$.epred) = draws_of(filtered_ref$.epred)[draw_subset,]
  dimnames(draws_of(filtered_ref$.epred))[[1]] = 1:10

  expect_equal(epred_rvars(m_hp_wt, mtcars_tbl, ndraws = 10, seed = 1234), filtered_ref)
  expect_equal(add_epred_rvars(mtcars_tbl, m_hp_wt, ndraws = 10, seed = 1234), filtered_ref)
})

test_that("[add_]epred_rvars works on an rstanarm model with grouped newdata", {
  skip_if_not_installed("rstanarm")
  m_hp_wt = readRDS(test_path("../models/models.rstanarm.m_hp_wt.rds"))

  ref = mtcars_tbl %>%
    mutate(.epred = rvar(rstantools::posterior_epred(m_hp_wt, newdata = mtcars_tbl))) %>%
    group_by(hp)

  expect_equal(epred_rvars(m_hp_wt, group_by(mtcars_tbl, hp)), ref)
  expect_equal(add_epred_rvars(group_by(mtcars_tbl, hp), m_hp_wt), ref)
})


test_that("[add_]epred_rvars works on brms models without dpar", {
  skip_if_not_installed("brms")
  m_hp = readRDS(test_path("../models/models.brms.m_hp.rds"))

  ref = mtcars_tbl %>%
    mutate(.epred = rvar(rstantools::posterior_epred(m_hp, newdata = mtcars_tbl)))

  expect_equal(epred_rvars(m_hp, mtcars_tbl), ref)
  expect_equal(add_epred_rvars(mtcars_tbl, m_hp), ref)
  expect_equal(add_epred_rvars(mtcars_tbl, m_hp, dpar = FALSE), ref)
  expect_equal(add_epred_rvars(mtcars_tbl, m_hp, dpar = FALSE, value = "foo"), rename(ref, foo = .epred))

  #epred_rvars.default should work fine here
  expect_equal(epred_rvars.default(m_hp, mtcars_tbl), ref)

  #subsetting to test the `ndraws` argument
  set.seed(1234)
  filtered_ref = mtcars_tbl %>%
    mutate(.epred = rvar(rstantools::posterior_epred(m_hp, newdata = mtcars_tbl, ndraws = 10)))

  expect_equal(add_epred_rvars(mtcars_tbl, m_hp, ndraws = 10, seed = 1234), filtered_ref)
})


test_that("[add_]epred_rvars works on brms models with dpar", {
  skip_if_not_installed("brms")
  m_hp_sigma = readRDS(test_path("../models/models.brms.m_hp_sigma.rds"))

  ref = mtcars_tbl %>%
    mutate(
      .epred = rvar(rstantools::posterior_epred(m_hp_sigma, newdata = mtcars_tbl)),
      mu = rvar(rstantools::posterior_epred(m_hp_sigma, newdata = mtcars_tbl, dpar = "mu")),
      sigma = rvar(rstantools::posterior_epred(m_hp_sigma, newdata = mtcars_tbl, dpar = "sigma"))
    )

  expect_equal(epred_rvars(m_hp_sigma, mtcars_tbl, dpar = TRUE), ref)
  expect_equal(add_epred_rvars(mtcars_tbl, m_hp_sigma, dpar = TRUE), ref)
  expect_equal(add_epred_rvars(mtcars_tbl, m_hp_sigma, dpar = "sigma"), select(ref, -mu))
  expect_equal(add_epred_rvars(mtcars_tbl, m_hp_sigma, dpar = "mu"), select(ref, -sigma))
  expect_equal(add_epred_rvars(mtcars_tbl, m_hp_sigma, dpar = FALSE), select(ref, -sigma, -mu))
  expect_equal(add_epred_rvars(mtcars_tbl, m_hp_sigma, dpar = NULL), select(ref, -sigma, -mu))
  expect_equal(add_epred_rvars(mtcars_tbl, m_hp_sigma, dpar = list("mu", "sigma", s1 = "sigma")), mutate(ref, s1 = sigma))


  #subsetting to test the `ndraws` argument
  filtered_ref = mtcars_tbl
  set.seed(1234)
  filtered_ref$.epred = rvar(rstantools::posterior_epred(m_hp_sigma, newdata = mtcars_tbl, ndraws = 10))
  set.seed(1234)
  filtered_ref$mu = rvar(rstantools::posterior_epred(m_hp_sigma, newdata = mtcars_tbl, dpar = "mu", ndraws = 10))
  set.seed(1234)
  filtered_ref$sigma = rvar(rstantools::posterior_epred(m_hp_sigma, newdata = mtcars_tbl, dpar = "sigma", ndraws = 10))

  expect_equal(add_epred_rvars(mtcars_tbl, m_hp_sigma, ndraws = 10, seed = 1234, dpar = TRUE), filtered_ref)
})


test_that("[add_]epred_rvars works on simple brms models with nlpars", {
  skip_if_not_installed("brms")
  m_nlpar = readRDS(test_path("../models/models.brms.m_nlpar.rds"))
  df_nlpar = as_tibble(m_nlpar$data)

  ref = df_nlpar %>%
    mutate(.epred = rvar(rstantools::posterior_epred(m_nlpar, newdata = df_nlpar)))

  expect_equal(epred_rvars(m_nlpar, df_nlpar), ref)
  expect_equal(add_epred_rvars(df_nlpar, m_nlpar), ref)
  expect_equal(add_epred_rvars(df_nlpar, m_nlpar, dpar = FALSE), ref)
})


test_that("[add_]epred_rvars works on simple brms models with multiple dpars", {
  skip_if_not_installed("brms")
  m_dpars = readRDS(test_path("../models/models.brms.m_dpars.rds"))
  df_dpars = as_tibble(m_dpars$data)

  ref = df_dpars %>%
    mutate(
      .epred = rvar(rstantools::posterior_epred(m_dpars, newdata = df_dpars)),
      mu1 = rvar(rstantools::posterior_epred(m_dpars, newdata = df_dpars, dpar = "mu1")),
      mu2 = rvar(rstantools::posterior_epred(m_dpars, newdata = df_dpars, dpar = "mu2"))
    )

  expect_equal(epred_rvars(m_dpars, df_dpars, dpar = TRUE), ref)
  expect_equal(add_epred_rvars(df_dpars, m_dpars, dpar = list("mu1", "mu2")), ref)
  expect_equal(add_epred_rvars(df_dpars, m_dpars, dpar = FALSE), select(ref, -mu1, -mu2))
})


test_that("[add_]epred_rvars works on brms models with ordinal outcomes (response scale)", {
  skip_if_not_installed("brms")
  m_cyl_mpg = readRDS(test_path("../models/models.brms.m_cyl_mpg.rds"))

  ref = mtcars_tbl %>%
    mutate(.epred = rvar(rstantools::posterior_epred(m_cyl_mpg, newdata = mtcars_tbl)))

  expect_equal(epred_rvars(m_cyl_mpg, mtcars_tbl), ref)
  expect_equal(add_epred_rvars(mtcars_tbl, m_cyl_mpg), ref)

  # column transformation
  column_ref = ref %>%
    mutate(.row = 1:n()) %>%
    group_by(across(-.epred)) %>%
    summarise(cyl_pred = colnames(.epred), .epred = t(.epred), .groups = "drop") %>%
    arrange(cyl_pred, .row)
  dim(column_ref$.epred) = length(column_ref$.epred)
  expect_equal(add_epred_rvars(mtcars_tbl, m_cyl_mpg, columns_to = "cyl_pred"), column_ref)
})


test_that("[add_]epred_rvars works on brms models with dirichlet outcomes (response scale)", {
  skip_if_not_installed("brms")
  skip_if_not(getRversion() >= "4")

  m_dirich = readRDS(test_path("../models/models.brms.m_dirich.rds"))

  grid = tibble(x = c("A", "B"))
  ref = grid %>%
    mutate(.epred = rvar(rstantools::posterior_epred(m_dirich, newdata = grid)))

  expect_equal(epred_rvars(m_dirich, grid), ref)
})


test_that("[add_]epred_rvars allows extraction of dpar on brms models with categorical outcomes (response scale)", {
  skip_if_not_installed("brms")
  m_cyl_mpg = readRDS(test_path("../models/models.brms.m_cyl_mpg.rds"))

  ref = mtcars_tbl %>%
    mutate(
      .epred = rvar(rstantools::posterior_epred(m_cyl_mpg, newdata = mtcars_tbl)),
      mu = rvar(rstantools::posterior_epred(m_cyl_mpg, newdata = mtcars_tbl, dpar = "mu")),
      disc = rvar(rstantools::posterior_epred(m_cyl_mpg, newdata = mtcars_tbl, dpar = "disc"))
    )

  expect_equal(epred_rvars(m_cyl_mpg, mtcars_tbl, dpar = TRUE), ref)
  ref$disc = NULL
  expect_equal(add_epred_rvars(mtcars_tbl, m_cyl_mpg, dpar = "mu"), ref)
})


# non-generic argument tests ----------------------------------------------

test_that("[add_]epred_rvars throws an error when re.form is called instead of re_formula in rstanarm", {
  skip_if_not_installed("rstanarm")
  m_hp_wt = readRDS(test_path("../models/models.rstanarm.m_hp_wt.rds"))

  expect_error(
    m_hp_wt %>% epred_rvars(mtcars_tbl, re.form = NULL),
    "`re.form.*.`re_formula`.*.See the documentation for additional details."
  )
  expect_error(
    mtcars_tbl %>% add_epred_rvars(m_hp_wt, re.form = NULL),
    "`re.form.*.`re_formula`.*.See the documentation for additional details."
  )
})


# unknown model type tests ------------------------------------------------

test_that("rethinking model usage refers user to tidybayes.rethinking", {
  m = structure(list(), class = "map2stan")
  expect_error(epred_rvars(m), "tidybayes.rethinking")
})
