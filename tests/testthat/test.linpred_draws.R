# Tests for [add_]linpred_draws
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


test_that("[add_]linpred_draws throws an error on unsupported models", {
  data("RankCorr", package = "ggdist")

  expect_error(linpred_draws(RankCorr, data.frame()),
    'no applicable method')
  expect_error(add_linpred_draws(data.frame(), RankCorr),
    'no applicable method')
})


test_that("[add_]linpred_draws works on a simple rstanarm model", {
  skip_if_not_installed("rstanarm")
  m_hp_wt = readRDS(test_path("../models/models.rstanarm.m_hp_wt.rds"))

  make_ref = function(draws = NULL) {
    fits = rstanarm::posterior_linpred(m_hp_wt, newdata = mtcars_tbl, draws = draws) %>%
      as.data.frame() %>%
      mutate(
        .chain = NA_integer_,
        .iteration = NA_integer_,
        .draw = seq_len(n())
      ) %>%
      gather(.row, .linpred, -.chain, -.iteration, -.draw) %>%
      as_tibble()

    mtcars_tbl %>%
      mutate(.row = rownames(.)) %>%
      inner_join(fits, by = ".row") %>%
      mutate(.row = as.integer(.row)) %>%
      group_by(mpg, cyl, disp, hp, drat, wt, qsec, vs, am, gear, carb, .row)
  }
  ref = make_ref()

  expect_equal(linpred_draws(m_hp_wt, mtcars_tbl), ref)
  expect_equal(add_linpred_draws(mtcars_tbl, m_hp_wt), ref)
  expect_equal(add_linpred_draws(mtcars_tbl, m_hp_wt, value = "foo"), rename(ref, foo = .linpred))

  #subsetting to test the `ndraws` argument
  set.seed(1234)
  filtered_ref = make_ref(draws = 10)

  expect_equal(linpred_draws(m_hp_wt, mtcars_tbl, ndraws = 10, seed = 1234), filtered_ref)
  expect_equal(add_linpred_draws(mtcars_tbl, m_hp_wt, ndraws = 10, seed = 1234), filtered_ref)

  # default implementation should still work here
  expect_equal(linpred_draws.default(m_hp_wt, mtcars_tbl, draws = 10, seed = 1234), filtered_ref)
})


test_that("[add_]linpred_draws works on brms models with dpar", {
  skip_if_not_installed("brms")
  m_hp_sigma = readRDS(test_path("../models/models.brms.m_hp_sigma.rds"))

  make_ref = function(seed = 1234, nsamples = NULL) {
    set.seed(seed)
    fits = rstantools::posterior_linpred(m_hp_sigma, newdata = mtcars_tbl, nsamples = nsamples) %>%
      as.data.frame() %>%
      set_names(seq_len(ncol(.))) %>%
      mutate(
        .chain = NA_integer_,
        .iteration = NA_integer_,
        .draw = seq_len(n())
      ) %>%
      gather(.row, .linpred, -.chain, -.iteration, -.draw) %>%
      as_tibble()

    set.seed(seed)
    fits$mu = rstantools::posterior_linpred(m_hp_sigma, newdata = mtcars_tbl, nsamples = nsamples, dpar = "mu") %>%
      as.data.frame() %>%
      gather(.row, mu) %$%
      mu

    set.seed(seed)
    fits$sigma = rstantools::posterior_linpred(m_hp_sigma, newdata = mtcars_tbl, nsamples = nsamples, dpar = "sigma") %>%
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

  expect_equal(linpred_draws(m_hp_sigma, mtcars_tbl, dpar = TRUE), ref)
  expect_equal(add_linpred_draws(mtcars_tbl, m_hp_sigma, dpar = TRUE), ref)
  expect_equal(add_linpred_draws(mtcars_tbl, m_hp_sigma, dpar = "sigma"), select(ref, -mu))
  expect_equal(add_linpred_draws(mtcars_tbl, m_hp_sigma, dpar = "mu"), select(ref, -sigma))
  expect_equal(add_linpred_draws(mtcars_tbl, m_hp_sigma, dpar = FALSE), select(ref, -sigma, -mu))
  expect_equal(add_linpred_draws(mtcars_tbl, m_hp_sigma, dpar = NULL), select(ref, -sigma, -mu))
  expect_equal(add_linpred_draws(mtcars_tbl, m_hp_sigma, dpar = list("mu", "sigma", s1 = "sigma")), mutate(ref, s1 = sigma))


  #subsetting to test the `ndraws` argument
  filtered_ref = make_ref(seed = 1234, nsamples = 10)

  expect_equal(add_linpred_draws(mtcars_tbl, m_hp_sigma, ndraws = 10, seed = 1234, dpar = TRUE), filtered_ref)
})


test_that("[add_]linpred_draws works on brms models with ordinal outcomes (linear scale)", {
  skip_if_not_installed("brms")
  m_cyl_mpg = readRDS(test_path("../models/models.brms.m_cyl_mpg.rds"))

  fits = rstantools::posterior_linpred(m_cyl_mpg, newdata = mtcars_tbl) %>%
    as.data.frame() %>%
    set_names(seq_len(ncol(.))) %>%
    mutate(
      .chain = NA_integer_,
      .iteration = NA_integer_,
      .draw = seq_len(n())
    ) %>%
    gather(.row, .linpred, -.chain, -.iteration, -.draw) %>%
    as_tibble()

  ref = mtcars_tbl %>%
    mutate(.row = rownames(.)) %>%
    inner_join(fits, by = ".row") %>%
    mutate(.row = as.integer(.row)) %>%
    group_by(mpg, cyl, disp, hp, drat, wt, qsec, vs, am, gear, carb, .row)

  expect_equal(linpred_draws(m_cyl_mpg, mtcars_tbl), ref)
  expect_equal(add_linpred_draws(mtcars_tbl, m_cyl_mpg), ref)

  #fitted_draws deprecation check
  expect_warning(
    expect_equal(fitted_draws(m_cyl_mpg, mtcars_tbl, scale = "linear"), rename(ref, .value = .linpred)),
    "fitted_draws.*deprecated.*epred_draws.*linpred_draws"
  )
  expect_warning(
    expect_equal(add_fitted_draws(mtcars_tbl, m_cyl_mpg, scale = "linear"), rename(ref, .value = .linpred)),
    "fitted_draws.*deprecated.*epred_draws.*linpred_draws"
  )
})


test_that("[add_]linpred_draws allows extraction of dpar on brms models with ordinal outcomes (linear scale)", {
  skip_if_not_installed("brms")
  m_cyl_mpg = readRDS(test_path("../models/models.brms.m_cyl_mpg.rds"))

  fits = rstantools::posterior_linpred(m_cyl_mpg, newdata = mtcars_tbl, scale = "linear") %>%
    as.data.frame() %>%
    set_names(seq_len(ncol(.))) %>%
    mutate(
      .chain = NA_integer_,
      .iteration = NA_integer_,
      .draw = seq_len(n())
    ) %>%
    gather(.row, .linpred, -.chain, -.iteration, -.draw) %>%
    as_tibble()

  fits$mu = rstantools::posterior_linpred(m_cyl_mpg, newdata = mtcars_tbl, dpar = "mu", scale = "linear") %>%
    as.data.frame() %>%
    gather(.row, mu) %$%
    mu

  fits$disc = rstantools::posterior_linpred(m_cyl_mpg, newdata = mtcars_tbl, dpar = "disc", scale = "linear") %>%
    as.data.frame() %>%
    gather(.row, disc) %$%
    disc

  ref = mtcars_tbl %>%
    mutate(.row = rownames(.)) %>%
    inner_join(fits, by = ".row") %>%
    mutate(.row = as.integer(.row)) %>%
    group_by(mpg, cyl, disp, hp, drat, wt, qsec, vs, am, gear, carb, .row)

  expect_equal(linpred_draws(m_cyl_mpg, mtcars_tbl, dpar = TRUE), ref)

  # fitted_draws deprecation check
  expect_warning(
    expect_equal(fitted_draws(m_cyl_mpg, mtcars_tbl, scale = "linear", dpar = TRUE), rename(ref, .value = .linpred)),
    "fitted_draws.*deprecated.*epred_draws.*linpred_draws"
  )

  # scale = "linear" is deprecated and should give a warning to use `transform`
  ref$disc = NULL
  expect_warning(
    expect_equal(add_linpred_draws(mtcars_tbl, m_cyl_mpg, scale = "linear", dpar = "mu"), ref),
    "`scale`.*deprecated.*`transform`"
  )
})


test_that("[add_]linpred_draws throws an error when nsamples is called instead of ndraws in brms", {
  skip_if_not_installed("brms")
  m_hp = readRDS(test_path("../models/models.brms.m_hp.rds"))

  expect_error(
    m_hp %>% linpred_draws(newdata = mtcars_tbl, nsamples = 10),
    "`nsamples.*.`ndraws`.*.See the documentation for additional details."
  )
  expect_error(
    m_hp %>% add_linpred_draws(newdata = mtcars_tbl, nsamples = 10),
    "`nsamples.*.`ndraws`.*.See the documentation for additional details."
  )
})

test_that("[add_]predicted_draws throws an error when re.form is called instead of re_formula in rstanarm", {
  skip_if_not_installed("rstanarm")
  m_hp_wt = readRDS(test_path("../models/models.rstanarm.m_hp_wt.rds"))

  expect_error(
    m_hp_wt %>% linpred_draws(newdata = mtcars_tbl, re.form = NULL),
    "`re.form.*.`re_formula`.*.See the documentation for additional details."
  )
  expect_error(
    m_hp_wt %>% add_linpred_draws(newdata = mtcars_tbl, re.form = NULL),
    "`re.form.*.`re_formula`.*.See the documentation for additional details."
  )
})
