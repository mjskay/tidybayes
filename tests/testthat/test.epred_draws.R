# Tests for [add_]epred_draws
#
# Author: mjskay
###############################################################################

suppressWarnings(suppressMessages({
  library(dplyr)
  library(tidyr)
  library(arrayhelpers)
  library(magrittr)
}))




# data
mtcars_tbl = mtcars %>%
  set_rownames(seq_len(nrow(.))) %>%
  as_tibble()

# for reliable testing, need to use only a single core (otherwise random
# numbers do not seem to always be reproducible on brms)
options(mc.cores = 1)


test_that("[add_]epred_draws throws an error on unsupported models", {
  data("RankCorr", package = "ggdist")

  expect_error(epred_draws(RankCorr, data.frame()),
    'no applicable method')
  expect_error(add_epred_draws(data.frame(), RankCorr),
    'no applicable method')
})


test_that("[add_]epred_draws works on a simple rstanarm model", {
  skip_if_not_installed("rstanarm")
  m_hp_wt = readRDS(test_path("../models/models.rstanarm.m_hp_wt.rds"))

  make_ref = function(draws = NULL) {
    fits = rstanarm::posterior_epred(m_hp_wt, newdata = mtcars_tbl, draws = draws) %>%
      as.data.frame() %>%
      mutate(
        .chain = NA_integer_,
        .iteration = NA_integer_,
        .draw = seq_len(n())
      ) %>%
      gather(.row, .epred, -.chain, -.iteration, -.draw) %>%
      as_tibble()

    mtcars_tbl %>%
      mutate(.row = rownames(.)) %>%
      inner_join(fits, by = ".row") %>%
      mutate(.row = as.integer(.row)) %>%
      group_by(mpg, cyl, disp, hp, drat, wt, qsec, vs, am, gear, carb, .row)
  }
  ref = make_ref()

  expect_equal(epred_draws(m_hp_wt, mtcars_tbl), ref)
  expect_equal(add_epred_draws(mtcars_tbl, m_hp_wt), ref)
  expect_equal(add_epred_draws(mtcars_tbl, m_hp_wt, value = "foo"), rename(ref, foo = .epred))

  # fitted_draws deprecation check
  expect_warning(
    expect_equal(add_fitted_draws(mtcars_tbl, m_hp_wt, value = "foo"), rename(ref, foo = .epred)),
    "fitted_draws.*deprecated.*epred_draws.*linpred_draws"
  )

  #subsetting to test the `ndraws` argument
  set.seed(1234)
  filtered_ref = make_ref(draws = 10)

  expect_equal(epred_draws(m_hp_wt, mtcars_tbl, ndraws = 10, seed = 1234), filtered_ref)
  expect_equal(add_epred_draws(mtcars_tbl, m_hp_wt, ndraws = 10, seed = 1234), filtered_ref)

  # default implementation should still work here
  expect_equal(epred_draws.default(m_hp_wt, mtcars_tbl, draws = 10, seed = 1234), filtered_ref)
})

test_that("[add_]epred_draws works on an rstanarm model with grouped newdata", {
  skip_if_not_installed("rstanarm")
  m_hp_wt = readRDS(test_path("../models/models.rstanarm.m_hp_wt.rds"))

  fits = rstanarm::posterior_epred(m_hp_wt, newdata = mtcars_tbl) %>%
    as.data.frame() %>%
    mutate(
      .chain = NA_integer_,
      .iteration = NA_integer_,
      .draw = seq_len(n())
    ) %>%
    gather(.row, .epred, -.chain, -.iteration, -.draw) %>%
    as_tibble()

  ref = mtcars_tbl %>%
    mutate(.row = rownames(.)) %>%
    inner_join(fits, by = ".row") %>%
    mutate(.row = as.integer(.row)) %>%
    group_by(mpg, cyl, disp, hp, drat, wt, qsec, vs, am, gear, carb, .row)

  expect_equal(epred_draws(m_hp_wt, group_by(mtcars_tbl, hp)), ref)
  expect_equal(add_epred_draws(mtcars_tbl, m_hp_wt), ref)
})


test_that("[add_]epred_draws works on brms models without dpar", {
  skip_if_not_installed("brms")
  m_hp = readRDS(test_path("../models/models.brms.m_hp.rds"))

  make_ref = function(ndraws = NULL) {
    fits = rstantools::posterior_epred(m_hp, mtcars_tbl, ndraws = ndraws) %>%
      as.data.frame() %>%
      set_names(seq_len(ncol(.))) %>%
      mutate(
        .chain = NA_integer_,
        .iteration = NA_integer_,
        .draw = seq_len(n())
      ) %>%
      gather(.row, .epred, -.chain, -.iteration, -.draw) %>%
      as_tibble()

    mtcars_tbl %>%
      mutate(.row = rownames(.)) %>%
      inner_join(fits, by = ".row") %>%
      mutate(.row = as.integer(.row)) %>%
      group_by(mpg, cyl, disp, hp, drat, wt, qsec, vs, am, gear, carb, .row)
  }
  ref = make_ref()

  expect_equal(epred_draws(m_hp, mtcars_tbl), ref)
  expect_equal(add_epred_draws(mtcars_tbl, m_hp), ref)
  expect_equal(add_epred_draws(mtcars_tbl, m_hp, dpar = FALSE), ref)
  expect_equal(add_epred_draws(mtcars_tbl, m_hp, dpar = FALSE, value = "foo"), rename(ref, foo = .epred))

  # fitted_draws deprecation check
  expect_warning(
    expect_equal(add_fitted_draws(mtcars_tbl, m_hp, dpar = FALSE, value = "foo"), rename(ref, foo = .epred)),
    "fitted_draws.*deprecated.*epred_draws.*linpred_draws"
  )

  #subsetting to test the `ndraws` argument
  set.seed(1234)
  filtered_ref = make_ref(ndraws = 10)

  expect_equal(add_epred_draws(mtcars_tbl, m_hp, ndraws = 10, seed = 1234), filtered_ref)
})


test_that("[add_]epred_draws works on brms models with dpar", {
  skip_if_not_installed("brms")
  m_hp_sigma = readRDS(test_path("../models/models.brms.m_hp_sigma.rds"))

  make_ref = function(seed = 1234, ndraws = NULL) {
    set.seed(seed)
    fits = rstantools::posterior_epred(m_hp_sigma, mtcars_tbl, ndraws = ndraws) %>%
      as.data.frame() %>%
      set_names(seq_len(ncol(.))) %>%
      mutate(
        .chain = NA_integer_,
        .iteration = NA_integer_,
        .draw = seq_len(n())
      ) %>%
      gather(.row, .epred, -.chain, -.iteration, -.draw) %>%
      as_tibble()

    set.seed(seed)
    fits$mu = rstantools::posterior_epred(m_hp_sigma, mtcars_tbl, ndraws = ndraws, dpar = "mu") %>%
      as.data.frame() %>%
      gather(.row, mu) %$%
      mu

    set.seed(seed)
    fits$sigma = rstantools::posterior_epred(m_hp_sigma, mtcars_tbl, ndraws = ndraws, dpar = "sigma") %>%
      as.data.frame() %>%
      gather(.row, sigma) %$%
      sigma

    mtcars_tbl %>%
      mutate(.row = rownames(.)) %>%
      inner_join(fits, by = ".row") %>%
      mutate(.row = as.integer(.row)) %>%
      group_by(mpg, cyl, disp, hp, drat, wt, qsec, vs, am, gear, carb, .row)
  }
  ref = make_ref()

  expect_equal(epred_draws(m_hp_sigma, mtcars_tbl, dpar = TRUE), ref)
  expect_equal(add_epred_draws(mtcars_tbl, m_hp_sigma, dpar = TRUE), ref)
  expect_equal(add_epred_draws(mtcars_tbl, m_hp_sigma, dpar = "sigma"), select(ref, -mu))
  expect_equal(add_epred_draws(mtcars_tbl, m_hp_sigma, dpar = "mu"), select(ref, -sigma))
  expect_equal(add_epred_draws(mtcars_tbl, m_hp_sigma, dpar = FALSE), select(ref, -sigma, -mu))
  expect_equal(add_epred_draws(mtcars_tbl, m_hp_sigma, dpar = NULL), select(ref, -sigma, -mu))
  expect_equal(add_epred_draws(mtcars_tbl, m_hp_sigma, dpar = list("mu", "sigma", s1 = "sigma")), mutate(ref, s1 = sigma))


  #subsetting to test the `ndraws` argument
  filtered_ref = make_ref(seed = 1234, ndraws = 10)

  expect_equal(add_epred_draws(mtcars_tbl, m_hp_sigma, ndraws = 10, seed = 1234, dpar = TRUE), filtered_ref)
})


test_that("[add_]epred_draws works on simple brms models with nlpars", {
  skip_if_not_installed("brms")
  m_nlpar = readRDS(test_path("../models/models.brms.m_nlpar.rds"))
  df_nlpar = as_tibble(m_nlpar$data)

  fits = rstantools::posterior_epred(m_nlpar, df_nlpar) %>%
    as.data.frame() %>%
    set_names(seq_len(ncol(.))) %>%
    mutate(
      .chain = NA_integer_,
      .iteration = NA_integer_,
      .draw = seq_len(n())
    ) %>%
    gather(.row, .epred, -.chain, -.iteration, -.draw) %>%
    as_tibble()

  ref = df_nlpar %>%
    mutate(.row = rownames(.)) %>%
    inner_join(fits, by = ".row") %>%
    mutate(.row = as.integer(.row)) %>%
    group_by(y, x, .row)

  expect_equal(epred_draws(m_nlpar, df_nlpar), ref)
  expect_equal(add_epred_draws(df_nlpar, m_nlpar), ref)
  expect_equal(add_epred_draws(df_nlpar, m_nlpar, dpar = FALSE), ref)
})


test_that("[add_]epred_draws works on simple brms models with multiple dpars", {
  skip_if_not_installed("brms")
  m_dpars = readRDS(test_path("../models/models.brms.m_dpars.rds"))
  df_dpars = as_tibble(m_dpars$data)

  fits = rstantools::posterior_epred(m_dpars, df_dpars) %>%
    as.data.frame() %>%
    set_names(seq_len(ncol(.))) %>%
    mutate(
      .chain = NA_integer_,
      .iteration = NA_integer_,
      .draw = seq_len(n())
    ) %>%
    gather(.row, .epred, -.chain, -.iteration, -.draw) %>%
    as_tibble()

  fits$mu1 = rstantools::posterior_epred(m_dpars, df_dpars, dpar = "mu1") %>%
    as.data.frame() %>%
    gather(.row, mu1) %$%
    mu1

  fits$mu2 = rstantools::posterior_epred(m_dpars, df_dpars, dpar = "mu2") %>%
    as.data.frame() %>%
    gather(.row, mu2) %$%
    mu2

  ref = df_dpars %>%
    mutate(.row = rownames(.)) %>%
    inner_join(fits, by = ".row") %>%
    mutate(.row = as.integer(.row)) %>%
    group_by(count, Age, visit, .row)

  expect_equal(epred_draws(m_dpars, df_dpars, dpar = TRUE), ref)
  expect_equal(add_epred_draws(df_dpars, m_dpars, dpar = list("mu1", "mu2")), ref)
  # brms leaves some extra attributes on the resulting df, just ignore those
  # by using expect_equivalent here
  expect_equivalent(add_epred_draws(df_dpars, m_dpars, dpar = FALSE), select(ref, -mu1, -mu2))
})


test_that("[add_]epred_draws works on brms models with ordinal outcomes (response scale)", {
  skip_if_not_installed("brms")
  m_cyl_mpg = readRDS(test_path("../models/models.brms.m_cyl_mpg.rds"))

  make_ref = function(ndraws = NULL) {
    fits = rstantools::posterior_epred(m_cyl_mpg, mtcars_tbl, ndraws = ndraws) %>%
      array2df(list(.draw = NA, .row = NA, .category = TRUE), label.x = ".epred") %>%
      mutate(
        .chain = NA_integer_,
        .iteration = NA_integer_,
        .row = as.integer(.row),
        .draw = as.integer(.draw)
      )

    inner_join(mtcars_tbl %>%
      mutate(.row = as.integer(rownames(.))), fits, by = ".row") %>%
      select(mpg:.row, .chain, .iteration, .draw, .category, .epred) %>%
      group_by(mpg, cyl, disp, hp, drat, wt, qsec, vs, am, gear, carb, .row, .category)
  }
  ref = make_ref()

  expect_equal(epred_draws(m_cyl_mpg, mtcars_tbl), ref)
  expect_equal(add_epred_draws(mtcars_tbl, m_cyl_mpg), ref)
  expect_equal(add_epred_draws(mtcars_tbl, m_cyl_mpg, category = "foo"), rename(ref, foo = .category))

  #subsetting to test the `ndraws` argument
  set.seed(1234)
  filtered_ref = make_ref(ndraws = 10)

  expect_equal(add_epred_draws(mtcars_tbl, m_cyl_mpg, ndraws = 10, seed = 1234), filtered_ref)

})


test_that("[add_]epred_draws works on brms models with dirichlet outcomes (response scale)", {
  skip_if_not_installed("brms")
  m_dirich = readRDS(test_path("../models/models.brms.m_dirich.rds"))

  grid = tibble(x = c("A", "B"))
  fits = rstantools::posterior_epred(m_dirich, grid) %>%
    array2df(list(.draw = NA, .row = NA, .category = TRUE), label.x = ".epred") %>%
    mutate(
      .chain = NA_integer_,
      .iteration = NA_integer_,
      .row = as.integer(.row),
      .draw = as.integer(.draw)
    )

  ref = inner_join(grid %>% mutate(.row = as.integer(rownames(.))), fits, by = ".row") %>%
    select(x, .row, .chain, .iteration, .draw, .category, .epred) %>%
    group_by(x, .row, .category)

  expect_equal(epred_draws(m_dirich, grid), ref)
})


test_that("[add_]epred_draws allows extraction of dpar on brms models with categorical outcomes (response scale)", {
  skip_if_not_installed("brms")
  m_cyl_mpg = readRDS(test_path("../models/models.brms.m_cyl_mpg.rds"))

  fits = rstantools::posterior_epred(m_cyl_mpg, mtcars_tbl) %>%
    array2df(list(.draw = NA, .row = NA, .category = TRUE), label.x = ".epred")

  mu_fits = rstantools::posterior_epred(m_cyl_mpg, mtcars_tbl, dpar = "mu") %>%
    array2df(list(.draw = NA, .row = NA), label.x = "mu")

  disc_fits = rstantools::posterior_epred(m_cyl_mpg, mtcars_tbl, dpar = "disc") %>%
    array2df(list(.draw = NA, .row = NA), label.x = "disc")

  ref = mtcars_tbl %>% mutate(.row = as.integer(rownames(.))) %>%
    inner_join(fits, by = ".row") %>%
    left_join(mu_fits, by = c(".row", ".draw")) %>%
    left_join(disc_fits, by = c(".row", ".draw")) %>%
    mutate(
      .chain = NA_integer_,
      .iteration = NA_integer_,
      .row = as.integer(.row),
      .draw = as.integer(.draw),
      .category = factor(.category)
    ) %>%
    select(mpg:.row, .chain, .iteration, .draw, .category, everything()) %>%
    group_by(mpg, cyl, disp, hp, drat, wt, qsec, vs, am, gear, carb, .row, .category)

  expect_equal(epred_draws(m_cyl_mpg, mtcars_tbl, dpar = TRUE), ref)
  ref$disc = NULL
  expect_equal(add_epred_draws(mtcars_tbl, m_cyl_mpg, dpar = "mu"), ref)
})


test_that("[add_]predicted_draws throws an error when re.form is called instead of re_formula in rstanarm", {
  skip_if_not_installed("rstanarm")
  m_hp_wt = readRDS(test_path("../models/models.rstanarm.m_hp_wt.rds"))

  expect_error(
    m_hp_wt %>% epred_draws(newdata = mtcars_tbl, re.form = NULL),
    "`re.form.*.`re_formula`.*.See the documentation for additional details."
  )
  expect_error(
    m_hp_wt %>% add_epred_draws(newdata = mtcars_tbl, re.form = NULL),
    "`re.form.*.`re_formula`.*.See the documentation for additional details."
  )
})
