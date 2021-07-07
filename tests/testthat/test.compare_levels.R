# Tests for compare_levels
#
# Author: mjskay
###############################################################################

library(dplyr)
library(tidyr)




ff_labels = c("a", "b", "c")

get_draws = function() {
  #observations of tau grouped by the factor ff (with levels ff_labels)
  data(RankCorr, package = "ggdist")
  rank_corr = RankCorr[[1]]
  bind_rows(lapply(1:3, function(i) {
    data.frame(
      .chain = as.integer(1),
      .iteration = seq_len(nrow(rank_corr)),
      .draw = seq_len(nrow(rank_corr)),
      ff = ff_labels[i],
      tau = as.vector(rank_corr[, paste0("tau[", i, "]")])
    )
  }))
}

test_that("pairwise level comparison works", {
  draws = get_draws()

  draws_wide = spread(draws, ff, tau)
  ref = bind_rows(lapply(combn(levels(factor(draws$ff)), 2, simplify = FALSE), function(levels.) {
    draws_wide$ff = paste(levels.[[2]], "-", levels.[[1]])
    draws_wide$tau = draws_wide[[levels.[[2]]]] - draws_wide[[levels.[[1]]]]
    draws_wide
  })) %>%
    select(-one_of(ff_labels)) %>%
    group_by(ff)

  expect_equal(compare_levels(draws, tau, by = ff, comparison = pairwise), ref)
  expect_equal(group_vars(compare_levels(draws, tau, by = ff, comparison = pairwise)), "ff")
  expect_equal(compare_levels(draws, tau, by = ff, comparison = "pairwise"), ref)
  expect_equal(compare_levels(group_by(mutate(draws, .row = 1), ff, .row), tau, by = ff, comparison = pairwise), ref)
})

test_that("ordered level comparison works", {
  draws = get_draws()

  draws_wide = spread(draws, ff, tau)
  ref = bind_rows(lapply(lapply(2:3, function(i) c(ff_labels[[i]], ff_labels[[i - 1]])), function(levels.) {
    draws_wide$ff = paste(levels.[[1]], "-", levels.[[2]])
    draws_wide$tau = draws_wide[[levels.[[1]]]] - draws_wide[[levels.[[2]]]]
    draws_wide
  })) %>%
    select(-one_of(ff_labels)) %>%
    group_by(ff)

  expect_equal(compare_levels(draws, tau, by = ff, comparison = ordered), ref)
  expect_equal(compare_levels(draws, tau, by = ff, comparison = "ordered"), ref)
})

test_that("control level comparison works", {
  draws = get_draws()

  draws_wide = spread(draws, ff, tau)
  ref = bind_rows(lapply(lapply(2:3, function(i) c(ff_labels[[i]], ff_labels[[1]])), function(levels.) {
    draws_wide$ff = paste(levels.[[1]], "-", levels.[[2]])
    draws_wide$tau = draws_wide[[levels.[[1]]]] - draws_wide[[levels.[[2]]]]
    draws_wide
  })) %>%
    select(-one_of(ff_labels)) %>%
    group_by(ff)

  expect_equal(compare_levels(draws, tau, by = ff, comparison = control), ref)
})

test_that("default level comparison selects the correct comparison depending on if `by` is ordered", {
  draws = get_draws()

  expect_equal(compare_levels(draws, tau, by = ff, comparison = default),
    compare_levels(draws, tau, by = ff, comparison = pairwise))

  draws$ff = ordered(draws$ff)

  expect_equal(compare_levels(draws, tau, by = ff, comparison = default),
    compare_levels(draws, tau, by = ff, comparison = ordered))
})

test_that("named functions are supported and named with their own name", {
  draws = get_draws()

  draws_wide = spread(draws, ff, tau)
  ref = bind_rows(lapply(lapply(2:3, function(i) c(ff_labels[[i]], ff_labels[[1]])), function(levels.) {
    draws_wide$ff = paste(levels.[[1]], "+", levels.[[2]])
    draws_wide$tau = draws_wide[[levels.[[1]]]] + draws_wide[[levels.[[2]]]]
    draws_wide
  })) %>%
    select(-one_of(ff_labels)) %>%
    group_by(ff)

  expect_equal(compare_levels(draws, tau, by = ff, fun = `+`, comparison = control), ref)
})

test_that("anonymous functions are supported and named with `:`", {
  draws = get_draws()

  draws_wide = spread(draws, ff, tau)
  ref = bind_rows(lapply(lapply(2:3, function(i) c(ff_labels[[i]], ff_labels[[1]])), function(levels.) {
    draws_wide$ff = paste(levels.[[1]], ":", levels.[[2]])
    draws_wide$tau = draws_wide[[levels.[[1]]]] + draws_wide[[levels.[[2]]]]
    draws_wide
  })) %>%
    select(-one_of(ff_labels)) %>%
    group_by(ff)

  expect_equal(compare_levels(draws, tau, by = ff, fun = function(x, y) x + y, comparison = control), ref)
})

test_that("custom comparisons of lists of character vectors are supported", {
  draws = get_draws()

  draws_wide = spread(draws, ff, tau)
  ref = bind_rows(lapply(list(c("a", "b"), c("a", "c")), function(levels.) {
    draws_wide$ff = paste(levels.[[1]], "-", levels.[[2]])
    draws_wide$tau = draws_wide[[levels.[[1]]]] - draws_wide[[levels.[[2]]]]
    draws_wide
  })) %>%
    select(-one_of(ff_labels)) %>%
    group_by(ff)

  expect_equal(compare_levels(draws, tau, by = ff, comparison = list(c("a", "b"), c("a", "c"))), ref)
})

test_that("custom comparisons of lists of unevaluated expressions are supported", {
  draws = get_draws()

  draws_wide = spread(draws, ff, tau)
  ref = bind_rows(lapply(list(quote(a + b), quote(exp(c - a))), function(levels.) {
    draws_wide$ff = deparse0(levels.)
    draws_wide$tau = eval(levels., draws_wide)
    draws_wide
  })) %>%
    select(-one_of(ff_labels)) %>%
    group_by(ff)

  expect_equal(compare_levels(draws, tau, by = ff, comparison = list(quote(a + b), quote(exp(c - a)))), ref)
  expect_equal(compare_levels(draws, tau, by = ff, comparison = rlang::exprs(a + b, exp(c - a))), ref)
  # test named comparisons
  ref[ref$ff == "a + b",]$ff = "comp1"
  ref[ref$ff == "exp(c - a)",]$ff = "comp2"
  expect_equal(compare_levels(draws, tau, by = ff, comparison = rlang::exprs(comp1 = a + b, comp2 = exp(c - a))), ref)
})

test_that("comparisons of subsets of levels of factors are supported", {
  draws = get_draws() %>%
    filter(ff %in% c("a", "c"))

  draws_wide = spread(draws, ff, tau)
  ref = bind_rows(lapply(combn(levels(factor(draws$ff)), 2, simplify = FALSE), function(levels.) {
    draws_wide$ff = paste(levels.[[2]], "-", levels.[[1]])
    draws_wide$tau = draws_wide[[levels.[[2]]]] - draws_wide[[levels.[[1]]]]
    draws_wide
  })) %>%
    select(-one_of(c("a", "c"))) %>%
    group_by(ff)

  expect_equal(compare_levels(draws, tau, by = ff, comparison = pairwise), ref)
})

test_that("extraneous columns are dropped before comparison", {
  draws = get_draws()

  draws_extra = draws %>%
    mutate(sd = 1 / sqrt(tau))  #use something that won't act as a clean index

  expect_equal(
    compare_levels(draws, tau, by = ff, comparison = pairwise),
    compare_levels(draws_extra, tau, by = ff, comparison = pairwise)
  )
})

test_that("compare_levels respects groups of input data frame", {
  draws = RankCorr %>%
    spread_draws(b[i,j]) %>%
    filter(i %in% 1:3, j %in% 1:3) %>%
    group_by(i, j)

  ref = bind_rows(lapply(split(draws, draws[["i"]]), function (d) {
    draws_wide = spread(d, j, b)
    bind_rows(lapply(combn(levels(factor(d$j)), 2, simplify = FALSE), function(levels.) {
      draws_wide$j = paste(levels.[[2]], "-", levels.[[1]])
      draws_wide$b = draws_wide[[levels.[[2]]]] - draws_wide[[levels.[[1]]]]
      draws_wide
    }))
  })) %>%
    select(-one_of(c("1", "2", "3"))) %>%
    group_by(i, j)

  result = compare_levels(draws, b, by = j)

  expect_equal(result, ref)
  expect_equal(group_vars(result), group_vars(ref))
})

# compare_levels on rvars -------------------------------------------------

test_that("pairwise level comparison works on rvars", {
  rvar_df = tibble(
    ff = c("a","b","c"),
    tau = as_draws_rvars(RankCorr)$tau
  )

  ref = tibble(
    ff = c("b - a", "c - a", "c - b"),
    tau = with(rvar_df, c(tau[2] - tau[1], tau[3] - tau[1], tau[3] - tau[2]))
  ) %>%
    group_by(ff)

  expect_equal(compare_levels(rvar_df, tau, by = ff, comparison = pairwise), ref)
})

test_that("ordered level comparison works on rvars", {
  rvar_df = tibble(
    ff = c("a","b","c"),
    tau = as_draws_rvars(RankCorr)$tau
  )

  ref = tibble(
    ff = c("b - a", "c - b"),
    tau = with(rvar_df, c(tau[2] - tau[1], tau[3] - tau[2]))
  ) %>%
    group_by(ff)

  expect_equal(compare_levels(rvar_df, tau, by = ff, comparison = ordered), ref)
})

test_that("named functions are supported on rvars", {
  rvar_df = tibble(
    ff = c("a","b","c"),
    tau = as_draws_rvars(RankCorr)$tau
  )

  ref = tibble(
    ff = c("b + a", "c + a"),
    tau = with(rvar_df, c(tau[2] + tau[1], tau[3] + tau[1]))
  ) %>%
    group_by(ff)

  expect_equal(compare_levels(rvar_df, tau, by = ff, fun = `+`, comparison = control), ref)
})

test_that("custom comparisons of lists of unevaluated expressions are supported on rvars", {
  rvar_df = tibble(
    ff = c("a","b","c"),
    tau = as_draws_rvars(RankCorr)$tau
  )

  ref = tibble(
    ff = c("a + b", "exp(c - a)"),
    tau = with(rvar_df, c(tau[1] + tau[2], exp(tau[3] - tau[1])))
  ) %>%
    group_by(ff)

  expect_equal(compare_levels(rvar_df, tau, by = ff, comparison = list(quote(a + b), quote(exp(c - a)))), ref)
})
