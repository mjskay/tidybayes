# [add_]residual_draws
#
# Author: mjskay
###############################################################################


#' @importFrom stats residuals

#' @rdname add_predicted_draws
#' @export
add_residual_draws = function(
  newdata, object, ...,
  value = ".residual", ndraws = NULL, seed = NULL, re_formula = NULL, category = ".category",
  # deprecated arguments
  n
) {
  ndraws = .Deprecated_argument_alias(ndraws, n)
  residual_draws(
    object = object, newdata = newdata, ...,
    value = value, ndraws = ndraws, seed = seed, re_formula = re_formula, category = category
  )
}

#' @rdname add_predicted_draws
#' @export
residual_draws = function(
  object, newdata, ...,
  value = ".residual", ndraws = NULL, seed = NULL, re_formula = NULL, category = ".category",
  # deprecated arguments
  n, residual
) {
  value = .Deprecated_argument_alias(value, residual)
  ndraws = .Deprecated_argument_alias(ndraws, n)
  # we need to update the argument list as well if there were deprecated
  # arguments or partial matching will assign `n` to `newdata`
  if (!missing(n) || !missing(residual)) {
    residual_draws(
      object = object, newdata = newdata, ...,
      value = value, ndraws = ndraws, seed = seed, re_formula = re_formula, category = category
    )
  } else {
    UseMethod("residual_draws")
  }
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
  value = ".residual", ndraws = NULL, seed = NULL, re_formula = NULL, category = ".category"
) {
  pred_draws_(
    .f = residuals, ...,
    object = object, newdata = newdata, output_name = value,
    seed = seed, ndraws = ndraws, re_formula = re_formula, category = category,
    summary = FALSE
  )
}
