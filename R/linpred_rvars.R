# [add_]epred_rvars
#
# Author: mjskay
###############################################################################


# linpred_rvars aliases -------------------------------------------------

#' @rdname add_predicted_rvars
#' @export
add_linpred_rvars = function(newdata, model, linpred = ".linpred", ..., n = NULL, seed = NULL, re_formula = NULL, dpar = NULL, columns_to = NULL) {
  linpred_rvars(model, newdata = newdata, linpred = linpred, ..., n = n, seed = seed, re_formula = re_formula, dpar = dpar, columns_to = columns_to)
}

#' @rdname add_predicted_rvars
#' @export
linpred_rvars = function(model, newdata, linpred = ".linpred", ..., n = NULL, seed = NULL, re_formula = NULL, dpar = NULL, columns_to = NULL) {
  UseMethod("linpred_rvars")
}


# linpred_rvars generics -------------------------------------------------

#' @rdname add_predicted_rvars
#' @export
linpred_rvars.default = function(model, newdata, linpred = ".linpred", ..., n = NULL, seed = NULL, re_formula = NULL, dpar = NULL, columns_to = NULL) {
  pred_rvars_default_(
    .f = rstantools::posterior_linpred,
    model = model, newdata = newdata, .value = linpred,
    ...,
    n = n, seed = seed, re_formula = re_formula, dpar = dpar, columns_to = columns_to
  )
}

#' @rdname add_predicted_rvars
#' @export
linpred_rvars.stanreg = function(model, newdata, linpred = ".linpred", ..., n = NULL, seed = NULL, re_formula = NULL, dpar = NULL, columns_to = NULL) {
  if (!requireNamespace("rstanarm", quietly = TRUE)) {
    stop("The `rstanarm` package is needed for `linpred_rvars` to support `stanreg` objects.", call. = FALSE) # nocov
  }

  stop_on_non_generic_arg_(
    names(enquos(...)), "[add_]linpred_rvars", re_formula = "re.form", n = "draws"
  )

  pred_rvars_(
    rstantools::posterior_linpred, model, newdata, .value = linpred, ...,
    draws = n, seed = seed, re.form = re_formula,
    dpar = NULL, # rstanarm does not support dpar
    columns_to = columns_to
  )
}

#' @rdname add_predicted_rvars
#' @importFrom rlang is_true is_false is_empty
#' @export
linpred_rvars.brmsfit = function(model, newdata, linpred = ".linpred", ..., n = NULL, seed = NULL, re_formula = NULL, dpar = NULL, columns_to = NULL) {
  if (!requireNamespace("brms", quietly = TRUE)) {
    stop("The `brms` package is needed for `linpred_rvars` to support `brmsfit` objects.", call. = FALSE) # nocov
  }

  stop_on_non_generic_arg_(
    names(enquos(...)), "[add_]linpred_rvars", n = "nsamples"
  )

  pred_rvars_(
    rstantools::posterior_linpred, model, newdata, .value = linpred, ...,
    nsamples = n, seed = seed, re_formula = re_formula, dpar = dpar, columns_to = columns_to
  )
}
