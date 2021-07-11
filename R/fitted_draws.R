# [add_]fitted_draws
#
# Author: mjskay
###############################################################################


# fitted_draws / linpred_draws aliases -------------------------------------------------

#' @rdname add_predicted_draws
#' @export
add_fitted_draws = function(
  newdata, object, ...,
  value = ".value", ndraws = NULL, seed = NULL, re_formula = NULL,
  category = ".category", dpar = NULL, scale = c("response", "linear"),
  # deprecated arguments
  n
) {
  ndraws = .Deprecated_argument_alias(ndraws, n)
  fitted_draws(
    object = object, newdata = newdata, ...,
    value = value, ndraws = ndraws, seed = seed, re_formula = re_formula,
    category = category, dpar = dpar, scale = scale
  )
}

#' @rdname add_predicted_draws
#' @export
fitted_draws = function(
  object, newdata, ...,
  value = ".value", ndraws = NULL, seed = NULL, re_formula = NULL,
  category = ".category", dpar = NULL, scale = c("response", "linear"),
  # deprecated arguments
  n
) {
  ndraws = .Deprecated_argument_alias(ndraws, n)
  # we need to update the argument list as well if there were deprecated
  # arguments or partial matching will assign `n` to `newdata`
  if (!missing(n)) {
    fitted_draws(
      object = object, newdata = newdata, ...,
      value = value, ndraws = ndraws, seed = seed, re_formula = re_formula,
      category = category, dpar = dpar, scale = scale
    )
  } else {
    UseMethod("fitted_draws")
  }
}

#' @rdname add_predicted_draws
#' @export
add_linpred_draws = function(
  newdata, object, ...,
  value = ".value", ndraws = NULL, seed = NULL, re_formula = NULL,
  category = ".category", dpar = NULL, scale = c("response", "linear")
) {
  fitted_draws(
    object = object, newdata = newdata, ...,
    value = value, ndraws = ndraws, seed = seed, re_formula = re_formula,
    category = category, dpar = dpar, scale = scale
  )
}

#' @rdname add_predicted_draws
#' @export
linpred_draws = function(
  object, newdata, ...,
  value = ".value", ndraws = NULL, seed = NULL, re_formula = NULL,
  category = ".category", dpar = NULL, scale = c("response", "linear")
) {
  fitted_draws(
    object = object, newdata = newdata, ...,
    value = value, ndraws = ndraws, seed = seed, re_formula = re_formula,
    category = category, dpar = dpar, scale = scale
  )
}

# fitted_draws generics -------------------------------------------------

#' @rdname add_predicted_draws
#' @export
fitted_draws.default = function(object, newdata, ...) {
  model_class = class(object)

  if (isTRUE(model_class %in% c("ulam", "quap", "map", "map2stan"))) {
    stop(
      "Models of type ", deparse0(model_class), " are not supported by base tidybayes.\n",
      "Install the `tidybayes.rethinking` package to enable support for these models:\n",
      "  devtools::install_github('mjskay/tidybayes.rethinking')"
    )
  }
  stop(
    "Models of type ", deparse0(model_class), " are not currently supported by `fitted_draws`.\n",
    "You might try using `add_draws()` for models that do not have explicit fit/prediction\n",
    "support; see help(\"add_draws\") for an example. See also help(\"tidybayes-models\") for\n",
    "more information on what functions are supported by what model types."
  )
}

#' @rdname add_predicted_draws
#' @export
fitted_draws.stanreg = function(
  object, newdata, ...,
  value = ".value", ndraws = NULL, seed = NULL, re_formula = NULL,
  category = ".category", dpar = NULL, scale = c("response", "linear")
) {
  transform = match.arg(scale) == "response" # TODO drop and just pass throgh transform

  stop_on_non_generic_arg_(
    names(enquos(...)), "[add_]fitted_draws", re_formula = "re.form", scale = "transform"
  )

  pred_draws_(
    rstanarm::posterior_linpred, ..., # TODO: switch to epred
    object = object, newdata = newdata, output_name = value,
    draws = ndraws, seed = seed, category = category, re.form = re_formula, transform = transform
  )
}

#' @rdname add_predicted_draws
#' @importFrom rlang is_true is_false is_empty
#' @importFrom dplyr select_at
#' @export
fitted_draws.brmsfit = function(
  object, newdata, ...,
  value = ".value", ndraws = NULL, seed = NULL, re_formula = NULL,
  category = ".category", dpar = NULL, scale = c("response", "linear")
) {
  scale = match.arg(scale) # TODO: remove

  stop_on_non_generic_arg_(
    names(enquos(...)), "[add_]fitted_draws", ndraws = "nsamples"
  )

  pred_draws_(
    fitted, ...,
    object = object, newdata = newdata, output_name = value,
    nsamples = ndraws, seed = seed, re_formula = re_formula, category = category, dpar = dpar, scale = scale,
    summary = FALSE # TODO: switch to epred vs linpred, drop scale and remove?
  )
}
