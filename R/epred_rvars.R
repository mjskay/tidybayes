# [add_]epred_rvars
#
# Author: mjskay
###############################################################################


# [add_]epred_rvars aliases -------------------------------------------------

#' @rdname add_predicted_rvars
#' @export
add_epred_rvars = function(
  newdata, object, ...,
  epred = ".epred", n = NULL, seed = NULL, re_formula = NULL, dpar = NULL, columns_to = NULL
) {
  epred_rvars(
    object = object, newdata = newdata, ...,
    epred = epred, n = n, seed = seed, re_formula = re_formula, dpar = dpar, columns_to = columns_to
  )
}

#' @rdname add_predicted_rvars
#' @export
epred_rvars = function(
  object, newdata, ...,
  epred = ".epred", n = NULL, seed = NULL, re_formula = NULL, dpar = NULL, columns_to = NULL
) {
  UseMethod("epred_rvars")
}


# epred_rvars generics -------------------------------------------------

#' @rdname add_predicted_rvars
#' @export
epred_rvars.default = function(
  object, newdata, ...,
  epred = ".epred", n = NULL, seed = NULL, re_formula = NULL, dpar = NULL, columns_to = NULL
) {
  pred_rvars_default_(
    .f = rstantools::posterior_epred, ...,
    object = object, newdata = newdata, output_name = epred,
    n = n, seed = seed, re_formula = re_formula, dpar = dpar, columns_to = columns_to
  )
}

#' @rdname add_predicted_rvars
#' @export
epred_rvars.stanreg = function(
  object, newdata, ...,
  epred = ".epred", n = NULL, seed = NULL, re_formula = NULL, dpar = NULL, columns_to = NULL
) {
  stop_on_non_generic_arg_(
    names(enquos(...)), "[add_]epred_rvars", re_formula = "re.form", n = "draws"
  )

  pred_rvars_(
    rstantools::posterior_epred, ...,
    object = object, newdata = newdata, output_name = epred,
    draws = n, seed = seed, re.form = re_formula,
    dpar = NULL, # rstanarm does not support dpar
    columns_to = columns_to
  )
}

#' @rdname add_predicted_rvars
#' @importFrom rlang is_true is_false is_empty
#' @export
epred_rvars.brmsfit = function(
  object, newdata, ...,
  epred = ".epred", n = NULL, seed = NULL, re_formula = NULL, dpar = NULL, columns_to = NULL
) {
  stop_on_non_generic_arg_(
    names(enquos(...)), "[add_]epred_rvars", n = "nsamples"
  )

  pred_rvars_(
    rstantools::posterior_epred, ...,
    object = object, newdata = newdata, output_name = epred,
    nsamples = n, seed = seed, re_formula = re_formula, dpar = dpar, columns_to = columns_to
  )
}
