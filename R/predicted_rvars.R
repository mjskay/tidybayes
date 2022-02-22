# [add_]predicted_rvars
#
# Author: mjskay
###############################################################################


# predicted_rvars / add_predicted_rvars ---------------------------------------

#' Add `rvar`s for the linear predictor, posterior expectation, posterior predictive, or residuals of a model to a data frame
#'
#' Given a data frame and a model, adds [`rvar`]s of draws from the linear/link-level predictor,
#' the expectation of the posterior predictive, or the posterior predictive to
#' the data frame.
#'
#' @templateVar pred_type rvars
#' @templateVar draws [`rvar`]s containing draws
#' @template details-pred
#' @template param-pred-newdata
#' @template param-pred-object
#' @template param-pred-value
#' @template param-pred-dots
#' @template param-ndraws
#' @template param-seed
#' @template param-pred-re_formula
#' @template param-pred-dpar
#' @param columns_to For *some* models, such as ordinal, multinomial, and multivariate models (notably, [brms::brm()] models but
#' *not* [rstanarm::stan_polr()] models), the column of predictions in the resulting data frame may include nested columns.
#' For example, for ordinal/multinomial models, these columns correspond to different categories of the response variable.
#' It may be more convenient to turn these nested columns into rows in the output; if this is desired, set
#' `columns_to` to a string representing the name of a column you would like the column names to be placed in.
#' In this case, a `.row` column will also be added to the result indicating which rows of the output
#' correspond to the same row in `newdata`.
#' See `vignette("tidy-posterior")` for examples of dealing with output ordinal models.
#' @return A data frame (actually, a [tibble][tibble::tibble]) equal to the input `newdata` with
#' additional columns added containing [`rvar`]s representing the requested predictions or fits.
#' @author Matthew Kay
#' @seealso [add_predicted_draws()] for the analogous functions that use a long-data-frame-of-draws
#' format instead of a data-frame-of-`rvar`s format. See [spread_rvars()] for manipulating posteriors directly.
#' @keywords manip
#' @examplesIf requireNamespace("brms", quietly = TRUE) && requireNamespace("modelr", quietly = TRUE)
#' \donttest{
#'
#' library(ggplot2)
#' library(dplyr)
#' library(posterior)
#' library(brms)
#' library(modelr)
#'
#' theme_set(theme_light())
#'
#' m_mpg = brm(mpg ~ hp * cyl, data = mtcars, family = lognormal(),
#'   # 1 chain / few iterations just so example runs quickly
#'   # do not use in practice
#'   chains = 1, iter = 500)
#'
#' # Look at mean predictions for some cars (epred) and compare to
#' # the exponeniated mu parameter of the lognormal distribution (linpred).
#' # Notice how they are NOT the same. This is because exp(mu) for a
#' # lognormal distribution is equal to its median, not its mean.
#' mtcars %>%
#'   select(hp, cyl, mpg) %>%
#'   add_epred_rvars(m_mpg) %>%
#'   add_linpred_rvars(m_mpg, value = "mu") %>%
#'   mutate(expmu = exp(mu), .epred - expmu)
#'
#' # plot intervals around conditional means (epred_rvars)
#' mtcars %>%
#'   group_by(cyl) %>%
#'   data_grid(hp = seq_range(hp, n = 101)) %>%
#'   add_epred_rvars(m_mpg) %>%
#'   ggplot(aes(x = hp, color = ordered(cyl), fill = ordered(cyl))) +
#'   stat_dist_lineribbon(aes(dist = .epred), .width = c(.95, .8, .5), alpha = 1/3) +
#'   geom_point(aes(y = mpg), data = mtcars) +
#'   scale_color_brewer(palette = "Dark2") +
#'   scale_fill_brewer(palette = "Set2")
#'
#' # plot posterior predictive intervals (predicted_rvars)
#' mtcars %>%
#'   group_by(cyl) %>%
#'   data_grid(hp = seq_range(hp, n = 101)) %>%
#'   add_predicted_rvars(m_mpg) %>%
#'   ggplot(aes(x = hp, color = ordered(cyl), fill = ordered(cyl))) +
#'   stat_dist_lineribbon(aes(dist = .prediction), .width = c(.95, .8, .5), alpha = 1/3) +
#'   geom_point(aes(y = mpg), data = mtcars) +
#'   scale_color_brewer(palette = "Dark2") +
#'   scale_fill_brewer(palette = "Set2")
#'
#' }
#' @importFrom posterior rvar
#' @name add_predicted_rvars
#' @export
add_predicted_rvars = function(
  newdata, object, ...,
  value = ".prediction", ndraws = NULL, seed = NULL, re_formula = NULL, columns_to = NULL
) {
  predicted_rvars(
    object = object, newdata = newdata, ...,
    value = value, ndraws = ndraws, seed = seed, re_formula = re_formula, columns_to = columns_to
  )
}

#' @rdname add_predicted_rvars
#' @export
predicted_rvars = function(
  object, newdata, ...,
  value = ".prediction", ndraws = NULL, seed = NULL, re_formula = NULL, columns_to = NULL
) {
  UseMethod("predicted_rvars")
}

#' @rdname add_predicted_rvars
#' @export
predicted_rvars.default = function(
  object, newdata, ...,
  value = ".prediction", seed = NULL, columns_to = NULL
) {
  pred_rvars_default_(
    .name = "predicted_rvars",
    .f = rstantools::posterior_predict, ...,
    object = object, newdata = newdata, output_name = value,
    seed = seed, columns_to = columns_to
  )
}

#' @rdname add_predicted_rvars
#' @export
predicted_rvars.stanreg = function(
  object, newdata, ...,
  value = ".prediction", ndraws = NULL, seed = NULL, re_formula = NULL, columns_to = NULL
) {
  stop_on_non_generic_arg_(
    names(enquos(...)), "[add_]predicted_rvars", re_formula = "re.form", ndraws = "draws"
  )

  pred_rvars_(
    .f = rstantools::posterior_predict, ...,
    object = object, newdata = newdata, output_name = value,
    draws = ndraws, seed = seed, re.form = re_formula,
    dpar = NULL, # posterior_predict does not support dpar
    columns_to = columns_to
  )
}

#' @rdname add_predicted_rvars
#' @export
predicted_rvars.brmsfit = function(
  object, newdata, ...,
  value = ".prediction", ndraws = NULL, seed = NULL, re_formula = NULL, columns_to = NULL
) {
  pred_rvars_(
    .f = rstantools::posterior_predict, ...,
    object = object, newdata = newdata, output_name = value,
    ndraws = ndraws, seed = seed, re_formula = re_formula,
    dpar = NULL, # posterior_predict does not support dpar
    columns_to = columns_to
  )
}



# helpers for rvar prediction functions ---------------------------------------

#' epred_rvars.default, predicted_rvars.default, etc
#' @noRd
pred_rvars_default_ = function(
  .name, .f, ...,
  object, newdata, output_name,
  seed = NULL, dpar = NULL, columns_to = NULL
) {
  if (!requireNamespace("rstantools", quietly = TRUE)) {
    stop0('Using `', .name, '` requires the `rstantools` package to be installed.') #nocov
  }
  model_class = class(object)
  if (isTRUE(model_class %in% c("ulam", "quap", "map", "map2stan"))) {
    stop0(
      "Models of type ", deparse0(model_class), " are not supported by basic tidybayes::", .name, ".\n",
      "Install the `tidybayes.rethinking` package to enable support for these models:\n",
      "  devtools::install_github('mjskay/tidybayes.rethinking')"
    )
  }

  pred_rvars_(
    .f = .f, ...,
    object = object, newdata = newdata, output_name = output_name,
    seed = seed, dpar = dpar, columns_to = columns_to
  )
}


#' add rvars of predictions from `object` to `newdata`. Handles dpars (if present)
#' and ensures that the same seed is set if multiple calls to the prediction
#' function need to be made, so that subsampling is consistent.
#' @param .f a prediction function like `posterior_predict`, `posterior_epred`, etc
#' @param object a model
#' @param newdata a data frame representing a prediction grid
#' @param output_name name of the output column
#' @param seed seed to set
#' @param dpar dpars from the model to include
#' @param columns_to name of column to move columns from the prediction output into
#' @noRd
pred_rvars_ = function(
  .f, ...,
  object, newdata, output_name,
  seed = NULL, dpar = NULL, columns_to = NULL
) {
  # get the names of distributional regression parameters to include
  dpars = get_model_dpars(object, dpar)

  # determine a seed we can use so that it is the same for each call to
  # to the prediction function for the dpars
  seed = seed %||% sample.int(.Machine$integer.max, 1)

  # get the rvars for the primary parameter
  out = if (is_tibble(newdata)) newdata else as_tibble(newdata)
  out[[output_name]] = withr::with_seed(seed, rvar(.f(
    object = object, newdata = newdata, ...
  )))

  # get rvars for the dpars
  for (i in seq_along(dpars)) {
    varname = names(dpars)[[i]]
    out[[varname]] = withr::with_seed(seed, rvar(.f(
      object = object, newdata = newdata, ..., dpar = dpars[[i]]
    )))
  }

  rvar_pred_columns_to(out, output_name, columns_to)
}

#' If the result of a prediction for one of the add_XXX_rvars has columns and
#' columns_to is set, turn it into columns
#' @param pred data frame of predictions
#' @param output_name (string) name of an rvar column in pred containing predictions
#' @param columns_to (string) name of a column to move columns of pred[[var]] into
#' @noRd
#' @importFrom tidyselect any_of
rvar_pred_columns_to = function(pred, output_name, columns_to) {
  var = pred[[output_name]]
  ncol_ = NCOL(var)
  if (is.null(columns_to) || ncol_ <= 1) return(pred)

  # first, repeat the other vars in the data frame for as many columns
  # as there are in the prediction variable
  nrow_ = NROW(pred)
  pred[[".row"]] = seq_len(nrow_)
  pred = vctrs::vec_rep(select(pred, -any_of(output_name)), ncol_)

  # then, add a variable with column values
  colnames_ = colnames(var) %||% seq_len(ncol_)
  pred[[columns_to]] = rep(colnames_, each = nrow_)

  # then, flatten the first two dimensions of the prediction variable
  dims = dim(var)
  dim(var) = c(prod(dims[c(1,2)]), dims[-c(1:2)])
  pred[[output_name]] = var

  pred
}
