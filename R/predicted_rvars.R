# [add_]predicted_rvars
#
# Author: mjskay
###############################################################################


# [add_]predicted_rvars ---------------------------------------------------

#' Add rvars for the linear predictor, posterior expectation, posterior predictive, or residuals of a model to a data frame
#'
#' Given a data frame and a model, adds [`rvar`]s of draws from the linear/link-level predictor,
#' the expectation of the posterior predictive, the posterior predictive, or the residuals of a model to
#' the data frame.
#'
#' `add_linpred_rvars()` adds [`rvar`]s containing draws from (possibly transformed) posterior **linear**
#' predictors (or "link-level" predictors) to the data.
#' It corresponds to [rstanarm::posterior_linpred()] or [brms::posterior_linpred()].
#'
#' `add_epred_rvars()` adds draws from **expectation** of the posterior predictive distribution to
#' the data.
#' It corresponds to [rstanarm::posterior_epred()] or [brms::posterior_epred()].
#'
#' `add_predicted_rvars()` adds draws from posterior predictive to
#' the data.
#' It corresponds to [rstanarm::posterior_predict()] or [brms::posterior_predict()].
#'
#' The corresponding functions without `add_` as a prefix are alternate spellings
#' with the opposite order of the first two arguments: e.g. `add_predicted_rvars()`
#' and `predicted_rvars()`. This facilitates use in data
#' processing pipelines that start either with a data frame or a model.
#'
#' @param newdata Data frame to generate predictions from.
#' @param model A supported Bayesian model fit that can provide fits and predictions. Supported models
#' are listed in the second section of [tidybayes-models]: *Models Supporting Prediction*. While other
#' functions in this package (like [spread_rvars()]) support a wider range of models, to work with
#' `add_fitted_rvars()` and `add_predicted_rvars()` a model must provide an interface for generating
#' predictions, thus more generic Bayesian modeling interfaces like `runjags` and `rstan` are not directly
#' supported for these functions (only wrappers around those languages that provide predictions, like `rstanarm`
#' and `brm`, are supported here).
#' @param value The name of the output column for `add_linpred_rvars()`; default `".value"`.
#' @param epred The name of the output column for `add_epred_rvars()`; default `".epred"`.
#' @param prediction The name of the output column for `add_predicted_rvars()`; default `".prediction"`.
#' @param residual The name of the output column for `add_residual_rvars()`; default `".residual"`.
#' @param ... Additional arguments passed to the underlying prediction method for the type of
#' model given.
#' @param n The number of draws per prediction / fit to return, or `NULL` to return all draws.
#' @param seed A seed to use when subsampling draws (i.e. when `n` is not `NULL`).
#' @param re_formula formula containing group-level effects to be considered in the prediction.
#' If `NULL` (default), include all group-level effects; if `NA`, include no group-level effects.
#' Some model types (such as [brms::brmsfit] and [rstanarm::stanreg-objects]) allow
#' marginalizing over grouping factors by specifying new levels of a factor in `newdata`. In the case of
#' [brms::brm()], you must also pass `allow_new_levels = TRUE` here to include new levels (see
#' [brms::posterior_predict()]).
#' @param dpar For `add_epred_rvars()`: Should distributional regression
#' parameters be included in the output? Valid only for models that support distributional regression parameters,
#' such as submodels for variance parameters (as in `brms::brm()`). If `TRUE`, distributional regression
#' parameters are included in the output as additional columns named after each parameter
#' (alternative names can be provided using a list or named vector, e.g. `c(sigma.hat = "sigma")`
#' would output the `"sigma"` parameter from a model as a column named `"sigma.hat"`).
#' If `NULL` or `FALSE` (the default), distributional regression parameters are not included.
#' @param columns_to For *some* models, such as ordinal, multinomial, and multivariate models (notably, [brms::brm()] models but
#' *not* [rstanarm::stan_polr()] models), the column of predictions in the resulting data frame may include nested columns.
#' For example, for ordinal/multinomial models, these columns correspond to different categories of the response variable.
#' It may be more convenient to turn these nested columns into rows in the output; if this is desired, set
#' `columns_to` to a string representing the name of a column you would like the column names to be placed in.
#' In this case, a `.row` column will also be added to the result indicating which rows of the output
#' correspond to the same row in `newdata`.
#' See `vignette("tidy-posterior")` for examples of dealing with output ordinal models.
#' @param scale Either `"response"` or `"linear"`. If `"response"`, results are returned
#' on the scale of the response variable. If `"linear"`, fitted values are returned on the scale of
#' the linear predictor.
#' @return A data frame (actually, a [tibble][tibble::tibble]) equal to the input `newdata` with
#' additional columns added containing [`rvar`]s representing the requested predictions or fits.
#' @author Matthew Kay
#' @seealso [add_predicted_draws()] for the analogous functions that use a long-data-frame-of-draws
#' format instead of a data-frame-of-rvars format. See [spread_rvars()] for manipulating posteriors directly.
#' @keywords manip
#' @examples
#' \donttest{
#'
#' library(ggplot2)
#' library(dplyr)
#' library(posterior)
#'
#' if (
#'   require("rstanarm", quietly = TRUE) &&
#'   require("modelr", quietly = TRUE)
#' ) {
#'
#'   theme_set(theme_light())
#'
#'   m_mpg = stan_glm(mpg ~ hp * cyl, data = mtcars,
#'     # 1 chain / few iterations just so example runs quickly
#'     # do not use in practice
#'     chains = 1, iter = 500)
#'
#'   # look at mean predictions for some cars
#'   mtcars %>%
#'     select(hp, cyl, mpg) %>%
#'     add_epred_rvars(m_mpg)
#'
#'   # plot intervals around conditional means
#'   mtcars %>%
#'     group_by(cyl) %>%
#'     data_grid(hp = seq_range(hp, n = 101)) %>%
#'     add_epred_rvars(m_mpg) %>%
#'     ggplot(aes(x = hp, color = ordered(cyl), fill = ordered(cyl))) +
#'     stat_dist_lineribbon(aes(dist = .epred), .width = c(.95, .8, .5), alpha = 1/3) +
#'     geom_point(aes(y = mpg), data = mtcars) +
#'     scale_color_brewer(palette = "Dark2") +
#'     scale_fill_brewer(palette = "Set2")
#'
#'   # plot posterior predictive intervals
#'   mtcars %>%
#'     group_by(cyl) %>%
#'     data_grid(hp = seq_range(hp, n = 101)) %>%
#'     add_predicted_rvars(m_mpg) %>%
#'     ggplot(aes(x = hp, color = ordered(cyl), fill = ordered(cyl))) +
#'     stat_dist_lineribbon(aes(dist = .prediction), .width = c(.95, .8, .5), alpha = 1/3) +
#'     geom_point(aes(y = mpg), data = mtcars) +
#'     scale_color_brewer(palette = "Dark2") +
#'     scale_fill_brewer(palette = "Set2")
#' }
#' }
#' @importFrom posterior rvar
#' @export
add_predicted_rvars = function(newdata, model, prediction = ".prediction", ..., n = NULL, seed = NULL, re_formula = NULL, columns_to = NULL) {
  predicted_rvars(model, newdata, prediction, ..., n = n, seed = seed, re_formula = re_formula, columns_to = columns_to)
}

#' @rdname add_predicted_rvars
#' @export
predicted_rvars = function(model, newdata, prediction = ".prediction", ..., n = NULL, seed = NULL, re_formula = NULL, columns_to = NULL) {
  UseMethod("predicted_rvars")
}

#' @rdname add_predicted_rvars
#' @export
predicted_rvars.default = function(model, newdata, prediction = ".prediction", ..., n = NULL, seed = NULL, re_formula = NULL, columns_to = NULL) {
  args = list(
    quote(model),
    quote(newdata)
  )
  # only set these if they aren't default (NULL) in case the underlying function
  # does not actually support that parameter
  for (arg in c("n", "seed", "re_formula", "dpar")) {
    arg_value = get(arg, inherits = FALSE)
    if (!is.null(arg_value)) {
      args[[arg]] = arg_value
    }
  }

  out = if (is_tibble(newdata)) newdata else as_tibble(newdata)
  out[[prediction]] = rvar(do.call(rstantools::posterior_predict, args))
  rvar_pred_columns_to(out, prediction, columns_to)
}

#' @rdname add_predicted_rvars
#' @export
predicted_rvars.stanreg = function(model, newdata, prediction = ".prediction", ..., n = NULL, seed = NULL, re_formula = NULL, columns_to = NULL) {
  if (!requireNamespace("rstanarm", quietly = TRUE)) {
    stop("The `rstanarm` package is needed for `predicted_rvars` to support `stanreg` objects.", call. = FALSE) # nocov
  }

  stop_on_non_generic_arg_(
    names(enquos(...)), "[add_]predicted_rvars", re_formula = "re.form", n = "draws"
  )

  out = if (is_tibble(newdata)) newdata else as_tibble(newdata)
  out[[prediction]] = rvar(rstantools::posterior_predict(model, newdata = newdata, ..., re.form = re_formula, draws = n, seed = seed))
  rvar_pred_columns_to(out, prediction, columns_to)
}

#' @rdname add_predicted_rvars
#' @export
predicted_rvars.brmsfit = function(model, newdata, prediction = ".prediction", ..., n = NULL, seed = NULL, re_formula = NULL, columns_to = NULL) {
  if (!requireNamespace("brms", quietly = TRUE)) {
    stop("The `brms` package is needed for `predicted_rvars` to support `brmsfit` objects.", call. = FALSE) # nocov
  }

  stop_on_non_generic_arg_(
    names(enquos(...)), "[add_]predicted_rvars", n = "nsamples"
  )

  if (!is.null(seed)) {
    set.seed(seed)
  }

  out = if (is_tibble(newdata)) newdata else as_tibble(newdata)
  out[[prediction]] = rvar(rstantools::posterior_predict(model, newdata = newdata, ..., re.form = re_formula, nsamples = n))
  rvar_pred_columns_to(out, prediction, columns_to)
}
