# [add_]residual_draws
#
# Author: mjskay
###############################################################################


#' @importFrom stats residuals

#' @rdname add_predicted_draws
#' @export
add_residual_draws = function(
  newdata, object, ...,
  residual = ".residual", n = NULL, seed = NULL, re_formula = NULL,
  category = ".category"
) {
  residual_draws(
    object = object, newdata = newdata, ...,
    residual = residual, n = n, seed = seed, re_formula = re_formula,
    category = category
  )
}

#' @rdname add_predicted_draws
#' @export
residual_draws = function(
  object, newdata, ...,
  residual = ".residual", n = NULL, seed = NULL, re_formula = NULL,
  category = ".category"
) {
  UseMethod("residual_draws")
}

#' @rdname add_predicted_draws
#' @export
residual_draws.default = function(object, newdata, ...) {
  stop(paste0("Models of type ", deparse0(class(object)), " are not currently supported by `residual_draws`"))
}

#' @rdname add_predicted_draws
#' @export
residual_draws.brmsfit = function(
  object, newdata, ...,
  residual = ".residual", n = NULL, seed = NULL, re_formula = NULL,
  category = ".category"
) {
  if (!requireNamespace("brms", quietly = TRUE)) {
    stop("The `brms` package is needed for `residual_draws` to support `brmsfit` objects.", call. = FALSE) # nocov
  }

  stop_on_non_generic_arg_(
    names(enquos(...)), "[add_]residual_draws", n = "nsamples"
  )

  set.seed(seed)
  fitted_predicted_draws_brmsfit_(
    residuals, ...,
    object = object, newdata = newdata, output_name = residual,
    nsamples = n, re_formula = re_formula, category = category
  )
}
