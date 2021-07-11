# [add_]predicted_draws
#
# Author: mjskay
###############################################################################


# predicted_draws / add_predicted_draws ---------------------------------------------------

#' Add draws from the posterior fit, predictions, or residuals of a model to a data frame
#'

#' Given a data frame and a model, adds draws from the linear/link-level predictor,
#' the expectation of the posterior predictive, the posterior predictive, or the residuals of a model to
#' the data frame in a long format.
#'
#' `add_epred_draws()` adds draws from **expectation** of the posterior predictive distribution to
#' the data.
#' It corresponds to [rstanarm::posterior_epred()] or [brms::posterior_epred()].
#'
#' `add_predicted_draws()` adds draws from posterior predictive distribution to
#' the data.
#' It corresponds to [rstanarm::posterior_predict()] or [brms::posterior_predict()].
#'
#' `add_linpred_draws()` adds draws from (possibly transformed) posterior **linear**
#' predictors (or "link-level" predictors) to the data.
#' It corresponds to [rstanarm::posterior_linpred()] or [brms::posterior_linpred()].
#'
#' `add_residual_draws()` adds draws from residuals to the data.
#' It corresponds to [brms::residuals.brmsfit()].
#'
#' The corresponding functions without `add_` as a prefix are alternate spellings
#' with the opposite order of the first two arguments: e.g. `add_predicted_draws()`
#' and `predicted_draws()`. This facilitates use in data
#' processing pipelines that start either with a data frame or a model.
#'
#' Given equal choice between the two, the spellings prefixed with `add_`
#' are preferred.
#'
#' @param newdata Data frame to generate predictions from. If omitted, most model types will
#' generate predictions from the data used to fit the model.
#' @param object A supported Bayesian model fit that can provide fits and predictions. Supported models
#' are listed in the second section of [tidybayes-models]: *Models Supporting Prediction*. While other
#' functions in this package (like [spread_draws()]) support a wider range of models, to work with
#' `add_fitted_draws` and `add_predicted_draws` a model must provide an interface for generating
#' predictions, thus more generic Bayesian modeling interfaces like `runjags` and `rstan` are not directly
#' supported for these functions (only wrappers around those languages that provide predictions, like `rstanarm`
#' and `brm`, are supported here).
#' @param epred The name of the output column for `epred_draws`; default `".epred"`.
#' @param prediction The name of the output column for `predicted_draws`; default `".prediction"`.
#' @param linpred The name of the output column for `linpred_draws`; default `".linpred"`.
#' @param residual The name of the output column for `residual_draws`; default `".residual"`.
#' @param ... Additional arguments passed to the underlying prediction method for the type of
#' model given.
#' @param ndraws The number of draws per prediction / fit to return, or `NULL` to return all draws.
#' @param seed A seed to use when subsampling draws (i.e. when `ndraws` is not `NULL`).
#' @param re_formula formula containing group-level effects to be considered in the prediction.
#' If `NULL` (default), include all group-level effects; if `NA`, include no group-level effects.
#' Some model types (such as [brms::brmsfit] and [rstanarm::stanreg-objects]) allow
#' marginalizing over grouping factors by specifying new levels of a factor in `newdata`. In the case of
#' [brms::brm()], you must also pass `allow_new_levels = TRUE` here to include new levels (see
#' [brms::predict.brmsfit()]).
#' @param category For *some* ordinal, multinomial, and multivariate models (notably, [brms::brm()] models but
#' *not* [rstanarm::stan_polr()] models), multiple sets of rows will be returned per input row for
#' `fitted_draws` or `predicted_draws`, depending on the model type. For ordinal/multinomial models,
#' these rows correspond to different categories of the response variable. For multivariate models, these correspond to
#' different response variables. The `category` argument specifies the name of the column
#' to put the category names (or variable names) into in the resulting data frame. The default name of this column
#' (`".category"`) reflects the fact that this functionality was originally used only for ordinal models and
#' has been re-used for multivariate models. The fact that multiple rows per response are returned only for some
#' model types reflects the fact that tidybayes takes the approach of tidying whatever output is given to us, and
#' the output from different modeling functions differs on this point.
#' See `vignette("tidy-brms")` and `vignette("tidy-rstanarm")` for examples of dealing with output
#' from ordinal models using both approaches.
#' @param dpar For `fitted_draws` and `add_fitted_draws`: Should distributional regression
#' parameters be included in the output? Valid only for models that support distributional regression parameters,
#' such as submodels for variance parameters (as in `brm`). If `TRUE`, distributional regression
#' parameters are included in the output as additional columns named after each parameter
#' (alternative names can be provided using a list or named vector, e.g. `c(sigma.hat = "sigma")`
#' would output the `"sigma"` parameter from a model as a column named `"sigma.hat"`).
#' If `FALSE` (the default), distributional regression parameters are not included.
#' @param n (Deprecated). Use `ndraws`.
#' @param value (Deprecated). Use `linpred`.
#' @param scale (Deprecated). Use the appropriate function (`epred_draws()` or `linpred_draws()`)
#' depending on what type of distirbution you want. For `linpred_draws()`, you may want the
#' `transform` argument. See `rstanarm::posterior_linpred()` or `brms::posterior_linpred()`.
#' @return A data frame (actually, a [tibble][tibble::tibble]) with a `.row` column (a
#' factor grouping rows from the input `newdata`), `.chain` column (the chain
#' each draw came from, or `NA` if the model does not provide chain information),
#' `.iteration` column (the iteration the draw came from, or `NA` if the model does
#' not provide iteration information), and a `.draw` column (a unique index corresponding to each draw
#' from the distribution). In addition, `fitted_draws` includes a column with its name specified by
#' the `value` argument (default is `.value`) containing draws from the (transformed) linear predictor,
#' and `predicted_draws` contains a `.prediction` column containing draws from the posterior predictive
#' distribution. For convenience, the resulting data frame comes grouped by the original input rows.
#' @author Matthew Kay
#' @seealso [add_draws()] for the variant of these functions for use with packages that do not have
#' explicit support for these functions yet. See [spread_draws()] for manipulating posteriors directly.
#' @keywords manip
#' @examples
#' \donttest{
#'
#' library(ggplot2)
#' library(dplyr)
#'
#' if (
#'   require("brms", quietly = TRUE) &&
#'   require("modelr", quietly = TRUE)
#' ) {
#'
#'   theme_set(theme_light())
#'
#'   m_mpg = brm(mpg ~ hp * cyl, data = mtcars,
#'     # 1 chain / few iterations just so example runs quickly
#'     # do not use in practice
#'     chains = 1, iter = 500)
#'
#'   # draw 100 fit lines from the posterior and overplot them
#'   print(mtcars %>%
#'     group_by(cyl) %>%
#'     data_grid(hp = seq_range(hp, n = 101)) %>%
#'     add_fitted_draws(m_mpg, ndraws = 100) %>%
#'     ggplot(aes(x = hp, y = mpg, color = ordered(cyl))) +
#'     geom_line(aes(y = .value, group = paste(cyl, .draw)), alpha = 0.25) +
#'     geom_point(data = mtcars)
#'   )
#'
#'   # plot posterior predictive intervals
#'   print(mtcars %>%
#'     group_by(cyl) %>%
#'     data_grid(hp = seq_range(hp, n = 101)) %>%
#'     add_predicted_draws(m_mpg) %>%
#'     ggplot(aes(x = hp, y = mpg, color = ordered(cyl))) +
#'     stat_lineribbon(aes(y = .prediction), .width = c(.99, .95, .8, .5), alpha = 0.25) +
#'     geom_point(data = mtcars) +
#'     scale_fill_brewer(palette = "Greys")
#'   )
#' }
#' }
#' @name add_predicted_draws
#' @importFrom magrittr %>%
#' @importFrom tidyr gather
#' @importFrom dplyr mutate sample_n ungroup group_by
#' @importFrom stats fitted predict
#' @importFrom rlang is_integerish
#' @export
add_predicted_draws = function(
  newdata, object, ...,
  prediction = ".prediction", ndraws = NULL, seed = NULL, re_formula = NULL, category = ".category",
  # deprecated arguments
  n
) {
  ndraws = .Deprecated_argument_alias(ndraws, n)
  predicted_draws(
    object = object, newdata = newdata, ...,
    prediction = prediction, ndraws = ndraws, seed = seed, re_formula = re_formula, category = category
  )
}

#' @rdname add_predicted_draws
#' @export
predicted_draws = function(
  object, newdata, ...,
  prediction = ".prediction", ndraws = NULL, seed = NULL, re_formula = NULL, category = ".category",
  # deprecated arguments
  n
) {
  ndraws = .Deprecated_argument_alias(ndraws, n)
  # we need to update the argument list as well when there are deprecated
  # arguments, otherwise partial matching might assign `n` to `newdata`
  if (!missing(n)) {
    predicted_draws(
      object = object, newdata = newdata, ...,
      prediction = prediction, ndraws = ndraws, seed = seed, re_formula = re_formula, category = category
    )
  } else {
    UseMethod("predicted_draws")
  }
}


# predicted_draws generics ------------------------------------------------

#' @rdname add_predicted_draws
#' @export
predicted_draws.default = function(
  object, newdata, ...,
  prediction = ".prediction", seed = NULL, category = ".category"
) {
  pred_draws_default_(
    .name = "predicted_draws",
    .f = rstantools::posterior_predict, ...,
    object = object, newdata = newdata, output_name = prediction,
    seed = seed, category = category
  )
}

#' @rdname add_predicted_draws
#' @export
predicted_draws.stanreg = function(
  object, newdata, ...,
  prediction = ".prediction", ndraws = NULL, seed = NULL, re_formula = NULL, category = ".category"
) {
  stop_on_non_generic_arg_(
    names(enquos(...)), "[add_]predicted_draws", ndraws = "draws", re_formula = "re.form"
  )

  pred_draws_(
    .f = rstantools::posterior_predict, ...,
    object = object, newdata = newdata, output_name = prediction,
    draws = ndraws, seed = seed, re.form = re_formula, category = category
  )
}

#' @rdname add_predicted_draws
#' @export
predicted_draws.brmsfit = function(
  object, newdata, ...,
  prediction = ".prediction", ndraws = NULL, seed = NULL, re_formula = NULL, category = ".category"
) {
  stop_on_non_generic_arg_(
    names(enquos(...)), "[add_]predicted_draws", ndraws = "nsamples"
  )

  pred_draws_(
    .f = rstantools::posterior_predict, ...,
    object = object, newdata = newdata, output_name = prediction,
    nsamples = ndraws, seed = seed, re_formula = re_formula, category = category
  )
}


# helpers for creating fits/predictions -----------------------------------

#' epred_draws.default, predicted_draws.default, etc
#' @noRd
pred_draws_default_ = function(
  .name, .f, ...,
  object, newdata, output_name,
  seed = NULL, dpar = NULL, category
) {
  if (!requireNamespace("rstantools", quietly = TRUE)) {
    stop0('Using `", .name, "` requires the `rstantools` package to be installed.') #nocov
  }
  model_class = class(object)
  if (isTRUE(model_class %in% c("ulam", "quap", "map", "map2stan"))) {
    stop0(
      "Models of type ", deparse0(model_class), " are not supported by basic tidybayes::", .name, ".\n",
      "Install the `tidybayes.rethinking` package to enable support for these models:\n",
      "  devtools::install_github('mjskay/tidybayes.rethinking')"
    )
  }

  pred_draws_(
    .f = .f, ...,
    object = object, newdata = newdata, output_name = output_name,
    seed = seed, dpar = dpar, category = category
  )
}

#' add draws of predictions from `object` to `newdata`. Handles dpars (if present)
#' and ensures that the same seed is set if multiple calls to the prediction
#' function need to be made, so that subsampling is consistent.
#' @param .f a prediction function like `posterior_predict`, `posterior_epred`, etc
#' @param object a model
#' @param newdata a data frame representing a prediction grid
#' @param output_name name of the output column
#' @param seed seed to set
#' @param dpar dpars from the model to include
#' @param category name of column to take category names from prediction
#' @noRd
pred_draws_ = function(
  .f, ...,
  object, newdata, output_name,
  seed = NULL, dpar = NULL, category
) {
  # get the names of distributional regression parameters to include
  dpars = get_model_dpars(object, dpar)

  # determine a seed we can use so that it is the same for each call to
  # to the prediction function for the dpars
  seed = seed %||% sample.int(.Machine$integer.max, 1)

  # get the draws for the primary parameter first so we can stick the other values onto it
  draws = withr::with_seed(seed, pred_draws_one_var_(
    .f, ...,
    object = object, newdata = newdata, output_name = output_name,
    category = category
  ))

  # stick draws from dpars onto primary parameter
  for (i in seq_along(dpars)) {
    varname = names(dpars)[[i]]
    dpar_fitted_draws = withr::with_seed(seed, pred_draws_one_var_(
      .f, ...,
      object = object, newdata = newdata, output_name = ".value",
      category = category, dpar = dpars[[i]]
    ))

    if (nrow(dpar_fitted_draws) == nrow(draws)) {
      draws[[varname]] = dpar_fitted_draws[[".value"]]
    } else {
      # in some models (such as ordinal models) the tidy draws from the dpars can have a different number
      # of rows than the linear predictor does if the linear predictor is on the response scale and the dpars are not.
      # In this case, we have to do a join to line things up (and in particular, a left join so that
      # rows from the linear predictor data frame are not dropped).
      join_cols = names(draws) %>%
        intersect(c(".row", ".draw", category)) %>%
        intersect(names(dpar_fitted_draws))

      dpar_fitted_draws %<>%
        ungroup() %>%
        select_at(c(join_cols, ".value")) %>%
        rename(!!varname := ".value")

      draws %<>% left_join(dpar_fitted_draws, by = join_cols)

      # stop(
      #   'Different number of rows in fitted draws for dpar "', dpars[[i]], '" and the linear predictor. This\n',
      #   'can happen in ordinal and categorical models when scale = "response". Try scale = "linear" instead.'
      # )
    }
  }

  draws
}

#' add draws of predictions from `object` to `newdata` for just one column of
#' predictions (the default prediction from `.f` or one dpar)
#' @param .f a prediction function like `posterior_predict`, `posterior_epred`, etc
#' @param object a model
#' @param newdata a data frame representing a prediction grid
#' @param output_name name of the output column
#' @param category name of column to move columns from the prediction output into
#' @noRd
#' @importFrom dplyr n
#' @importFrom arrayhelpers array2df ndim
pred_draws_one_var_ = function(
  .f, ...,
  object, newdata, output_name, category
) {
  newdata %<>% ungroup()

  column_format = list(
    .draw = NA,        #NA here means numeric
    .row = NA
  )

  fits_preds = .f(object = object, newdata = newdata, ...)

  groups = union(colnames(newdata), ".row")

  has_category = FALSE
  if (ndim(fits_preds) == 3) {
    #3 dimensions implies a categorical outcome, add a column for it
    # N.B.: at some point getting category names to work would be nice, but may be kind of brittle
    column_format[[3]] = TRUE
    names(column_format)[[3]] = category
    groups %<>% union(category)
    # we need to remember that there are categories, because when there are categories
    # the response variable should not be re-labelled according to categories
    has_category = TRUE
  }

  fits_preds_df = array2df(fits_preds, column_format, label.x = output_name)

  #rstanarm does something weird that prevents array2df from properly seeing .row and .draw as numerics,
  #so we have to convert them manually from factors. While we're at it, we should also make sure they are integers.
  if (is.factor(fits_preds_df$.row)) {
    fits_preds_df$.row = as.character(fits_preds_df$.row)
  }
  fits_preds_df$.row = as.integer(fits_preds_df$.row)

  if (is.factor(fits_preds_df$.draw)) {
    fits_preds_df$.draw = as.character(fits_preds_df$.draw)
  }
  fits_preds_df$.draw = as.integer(fits_preds_df$.draw)

  #for predictions from categorical models in brms, we can use the "levels" attribute
  #to recover the original factor levels. But we must be careful: dirichlet models and multinomial
  #models also get this attribute set, so we must also test that there is not already a `category`
  #column (has_category) and that responses are all positive integer values.
  prediction_levels = attr(fits_preds, "levels", exact = TRUE)
  if (!has_category &
      !is.null(prediction_levels) &
      is_integerish(fits_preds_df[[output_name]])
    ) {
    fits_preds_df[[output_name]] = factor(
      fits_preds_df[[output_name]],
      levels = seq_along(prediction_levels),
      labels = prediction_levels
    )
  }

  newdata %>%
    mutate(
      .row = seq_len(n()),
      .chain = NA_integer_,
      .iteration = NA_integer_
    ) %>%
    inner_join(fits_preds_df, by = ".row") %>%
    select(-!!sym(output_name), !!sym(output_name)) %>%
    group_by_at(groups)
}

#' Given a brms model and a dpar argument for linpred_draws()/etc, return a list of dpars
#' @noRd
get_model_dpars = function(object, dpar) {
  # only brms models support dpars at the moment
  if (!inherits(object, "brmsfit")) return(NULL)

  dpars = if (is_true(dpar)) {
    union(names(brms::brmsterms(object$formula)$dpar), object$family$dpars)
  } else if (is_false(dpar)) {
    NULL
  } else {
    dpar
  }
  if (is_empty(dpars)) {
    # the above conditions might return an empty vector, which does not play well with the code below
    # (if there are no dpars, it is expected that dpars is NULL)
    dpars = NULL
  }

  # missing names default to the same name used for the parameter in the model
  if (is.null(names(dpars))) {
    names(dpars) = dpars
  } else {
    missing_names = is.na(names(dpars)) | names(dpars) == ""
    names(dpars)[missing_names] = dpars[missing_names]
  }

  dpars
}
