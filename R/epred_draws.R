# [add_]epred_draws
#
# Author: mjskay
###############################################################################


# epred_draws aliases -------------------------------------------------

#' @rdname add_predicted_draws
#' @export
add_epred_draws = function(
  newdata, object, ...,
  epred = ".epred", n = NULL, seed = NULL, re_formula = NULL,
  category = ".category", dpar = NULL
) {
  epred_draws(
    object = object, newdata = newdata, ...,
    epred = epred, n = n, seed = seed, re_formula = re_formula,
    category = category, dpar = dpar
  )
}

#' @rdname add_predicted_draws
#' @export
epred_draws = function(
  object, newdata, ...,
  epred = ".epred", n = NULL, seed = NULL, re_formula = NULL,
  category = ".category", dpar = NULL
) {
  UseMethod("epred_draws")
}


# epred_draws generics -------------------------------------------------

#' @rdname add_predicted_draws
#' @export
epred_draws.default = function(object, newdata, ...) {
  model_class = class(object)

  if (model_class %in% c("ulam", "quap", "map", "map2stan")) {
    stop(
      "Models of type ", deparse0(model_class), " are not supported by base tidybayes.\n",
      "Install the `tidybayes.rethinking` package to enable support for these models:\n",
      "  devtools::install_github('mjskay/tidybayes.rethinking')"
    )
  }
  stop(
    "Models of type ", deparse0(model_class), " are not currently supported by `epred_draws`.\n",
    "You might try using `add_draws()` for models that do not have explicit fit/prediction\n",
    "support; see help(\"add_draws\") for an example. See also help(\"tidybayes-models\") for\n",
    "more information on what functions are supported by what model types."
  )
}

#' @rdname add_predicted_draws
#' @export
epred_draws.stanreg = function(
  object, newdata, ...,
  epred = ".epred", n = NULL, seed = NULL, re_formula = NULL,
  category = ".category", dpar = NULL
) {
  stop_on_non_generic_arg_(
    names(enquos(...)), "[add_]epred_draws", re_formula = "re.form"
  )

  pred_draws_(
    rstantools::posterior_epred, ...,
    object = object, newdata = newdata, output_name = epred,
    draws = n, seed = seed, category = category, re.form = re_formula
  )
}

#' @rdname add_predicted_draws
#' @importFrom rlang is_true is_false is_empty
#' @importFrom dplyr select_at
#' @export
epred_draws.brmsfit = function(
  object, newdata, ...,
  epred = ".epred", n = NULL, seed = NULL, re_formula = NULL,
  category = ".category", dpar = NULL
) {
  stop_on_non_generic_arg_(
    names(enquos(...)), "[add_]epred_draws", n = "nsamples"
  )

  pred_draws_(
    rstantools::posterior_epred, ...,
    object = object, newdata = newdata, output_name = epred,
    nsamples = n, seed = seed, re_formula = re_formula, category = category, dpar = dpar
  )
}
