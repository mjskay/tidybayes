# [add_]epred_rvars
#
# Author: mjskay
###############################################################################


# linpred_rvars aliases -------------------------------------------------

#' @rdname add_predicted_rvars
#' @export
add_linpred_rvars = function(
  newdata, object, ...,
  value = ".linpred", ndraws = NULL, seed = NULL, re_formula = NULL, dpar = NULL, columns_to = NULL
) {
  linpred_rvars(
    object = object, newdata = newdata, ...,
    value = value, ndraws = ndraws, seed = seed, re_formula = re_formula, dpar = dpar, columns_to = columns_to
  )
}

#' @rdname add_predicted_rvars
#' @export
linpred_rvars = function(
  object, newdata, ...,
  value = ".linpred", ndraws = NULL, seed = NULL, re_formula = NULL, dpar = NULL, columns_to = NULL
) {
  UseMethod("linpred_rvars")
}


# linpred_rvars generics -------------------------------------------------

#' @rdname add_predicted_rvars
#' @export
linpred_rvars.default = function(
  object, newdata, ...,
  value = ".linpred", seed = NULL, dpar = NULL, columns_to = NULL
) {
  pred_rvars_default_(
    .name = "linpred_rvars",
    .f = rstantools::posterior_linpred, ...,
    object = object, newdata = newdata, output_name = value,
    seed = seed, dpar = dpar, columns_to = columns_to
  )
}

#' @rdname add_predicted_rvars
#' @export
linpred_rvars.stanreg = function(
  object, newdata, ...,
  value = ".linpred", ndraws = NULL, seed = NULL, re_formula = NULL, dpar = NULL, columns_to = NULL
) {
  stop_on_non_generic_arg_(
    names(enquos(...)), "[add_]linpred_rvars", re_formula = "re.form", ndraws = "draws"
  )

  pred_rvars_(
    .f = rstantools::posterior_linpred, ...,
    object = object, newdata = newdata, output_name = value,
    draws = ndraws, seed = seed, re.form = re_formula,
    dpar = NULL, # rstanarm does not support dpar
    columns_to = columns_to
  )
}

#' @rdname add_predicted_rvars
#' @importFrom rlang is_true is_false is_empty
#' @export
linpred_rvars.brmsfit = function(
  object, newdata, ...,
  value = ".linpred", ndraws = NULL, seed = NULL, re_formula = NULL, dpar = NULL, columns_to = NULL
) {
  pred_rvars_(
    .f = rstantools::posterior_linpred, ...,
    object = object, newdata = newdata, output_name = value,
    ndraws = ndraws, seed = seed, re_formula = re_formula, dpar = dpar, columns_to = columns_to
  )
}
