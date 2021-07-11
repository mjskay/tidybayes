# [add_]epred_rvars
#
# Author: mjskay
###############################################################################


# epred_rvars / add_epred_rvars aliases -------------------------------------------------

#' @rdname add_predicted_rvars
#' @export
add_epred_rvars = function(
  newdata, object, ...,
  value = ".epred", ndraws = NULL, seed = NULL, re_formula = NULL, dpar = NULL, columns_to = NULL
) {
  epred_rvars(
    object = object, newdata = newdata, ...,
    value = value, ndraws = ndraws, seed = seed, re_formula = re_formula, dpar = dpar, columns_to = columns_to
  )
}

#' @rdname add_predicted_rvars
#' @export
epred_rvars = function(
  object, newdata, ...,
  value = ".epred", ndraws = NULL, seed = NULL, re_formula = NULL, dpar = NULL, columns_to = NULL
) {
  UseMethod("epred_rvars")
}


# epred_rvars generics -------------------------------------------------

#' @rdname add_predicted_rvars
#' @export
epred_rvars.default = function(
  object, newdata, ...,
  value = ".epred", seed = NULL, dpar = NULL, columns_to = NULL
) {
  pred_rvars_default_(
    .name = "epred_rvars",
    .f = rstantools::posterior_epred, ...,
    object = object, newdata = newdata, output_name = value,
    seed = seed, dpar = dpar, columns_to = columns_to
  )
}

#' @rdname add_predicted_rvars
#' @export
epred_rvars.stanreg = function(
  object, newdata, ...,
  value = ".epred", ndraws = NULL, seed = NULL, re_formula = NULL, dpar = NULL, columns_to = NULL
) {
  stop_on_non_generic_arg_(
    names(enquos(...)), "[add_]epred_rvars", re_formula = "re.form", ndraws = "draws"
  )

  pred_rvars_(
    .f = rstantools::posterior_epred, ...,
    object = object, newdata = newdata, output_name = value,
    draws = ndraws, seed = seed, re.form = re_formula,
    dpar = NULL, # rstanarm does not support dpar
    columns_to = columns_to
  )
}

#' @rdname add_predicted_rvars
#' @importFrom rlang is_true is_false is_empty
#' @export
epred_rvars.brmsfit = function(
  object, newdata, ...,
  value = ".epred", ndraws = NULL, seed = NULL, re_formula = NULL, dpar = NULL, columns_to = NULL
) {
  stop_on_non_generic_arg_(
    names(enquos(...)), "[add_]epred_rvars", ndraws = "nsamples"
  )

  pred_rvars_(
    .f = rstantools::posterior_epred, ...,
    object = object, newdata = newdata, output_name = value,
    nsamples = ndraws, seed = seed, re_formula = re_formula, dpar = dpar, columns_to = columns_to
  )
}
