# [add_]epred_rvars
#
# Author: mjskay
###############################################################################


# [add_]epred_rvars aliases -------------------------------------------------

#' @rdname add_predicted_rvars
#' @export
add_epred_rvars = function(newdata, model, epred = ".epred", ..., n = NULL, seed = NULL, re_formula = NULL, dpar = NULL, columns_to = NULL) {
  epred_rvars(model, newdata = newdata, epred = epred, ..., n = n, seed = seed, re_formula = re_formula, dpar = dpar, columns_to = columns_to)
}

#' @rdname add_predicted_rvars
#' @export
epred_rvars = function(model, newdata, epred = ".epred", ..., n = NULL, seed = NULL, re_formula = NULL, dpar = NULL, columns_to = NULL) {
  UseMethod("epred_rvars")
}


# epred_rvars generics -------------------------------------------------

#' @rdname add_predicted_rvars
#' @export
epred_rvars.default = function(model, newdata, epred = ".epred", ..., n = NULL, seed = NULL, re_formula = NULL, dpar = NULL, columns_to = NULL) {
  args = list(
    quote(model),
    quote(newdata)
  )
  # only set these if they aren't default (NULL) in case the underlying function
  # does not actually support that parameter
  for (arg in c("n", "seed", "re_formula", "dpar")) {
    arg_value = get(arg, inherits = FALSE)
    if (!is.null(arg_value)) {
      args[[arg]] = arg_value
    }
  }

  out = if (is_tibble(newdata)) newdata else as_tibble(newdata)
  out[[epred]] = rvar(do.call(rstantools::posterior_epred, args))
  rvar_pred_columns_to(out, epred, columns_to)
}

#' @rdname add_predicted_rvars
#' @export
epred_rvars.stanreg = function(model, newdata, epred = ".epred", ..., n = NULL, seed = NULL, re_formula = NULL, dpar = NULL, columns_to = NULL) {
  if (!requireNamespace("rstanarm", quietly = TRUE)) {
    stop("The `rstanarm` package is needed for `epred_rvars` to support `stanreg` objects.", call. = FALSE) # nocov
  }

  stop_on_non_generic_arg_(
    names(enquos(...)), "[add_]epred_rvars", re_formula = "re.form", n = "draws"
  )

  if (!is.null(seed)) {
    set.seed(seed)
  }

  out = if (is_tibble(newdata)) newdata else as_tibble(newdata)
  out[[epred]] = rvar(rstantools::posterior_epred(model, newdata = newdata, ..., re.form = re_formula, draws = n))
  rvar_pred_columns_to(out, epred, columns_to)
}

#' @rdname add_predicted_rvars
#' @importFrom rlang is_true is_false is_empty
#' @export
epred_rvars.brmsfit = function(model, newdata, epred = ".epred", ..., n = NULL, seed = NULL, re_formula = NULL, dpar = NULL, columns_to = NULL) {
  if (!requireNamespace("brms", quietly = TRUE)) {
    stop("The `brms` package is needed for `epred_rvars` to support `brmsfit` objects.", call. = FALSE) # nocov
  }

  stop_on_non_generic_arg_(
    names(enquos(...)), "[add_]epred_rvars", n = "nsamples", dpar = "dpars"
  )

  # get the names of distributional regression parameters to include
  dpars = get_brms_dpars(model, dpar)

  # determine a seed we can use so that it is the same for each call to
  # to posterior_epred for the dpars
  seed = seed %||% sample.int(.Machine$integer.max, 1)

  # get the rvars for the primary parameter
  out = if (is_tibble(newdata)) newdata else as_tibble(newdata)
  out[[epred]] = withr::with_seed(seed, rvar(rstantools::posterior_epred(
    model, newdata, ..., re_formula = re_formula, dpar = NULL, nsamples = n
  )))

  # get rvars for the dpars
  for (i in seq_along(dpars)) {
    varname = names(dpars)[[i]]
    out[[varname]] = withr::with_seed(seed, rvar(rstantools::posterior_epred(
      model, newdata, ..., re_formula = re_formula, dpar = dpars[[i]], nsamples = n
    )))
  }

  rvar_pred_columns_to(out, epred, columns_to)
}



# helpers -----------------------------------------------------------------

#' If the result of a prediction for one of the add_XXX_rvars has columns and
#' columns_to is set, turn it into columns
#' @param pred data frame of predictions
#' @param varname (string) name of an rvar column in pred containing predictions
#' @param columns_to (string) name of a column to move columns of pred[[var]] into
#' @noRd
#' @importFrom tidyselect any_of
rvar_pred_columns_to = function(pred, varname, columns_to) {
  var = pred[[varname]]
  ncol_ = NCOL(var)
  if (is.null(columns_to) || ncol_ <= 1) return(pred)

  # first, repeat the other vars in the data frame for as many columns
  # as there are in the prediction variable
  nrow_ = NROW(pred)
  pred[[".row"]] = seq_len(nrow_)
  pred = vctrs::vec_rep(select(pred, -any_of(varname)), ncol_)

  # then, add a variable with column values
  colnames_ = colnames(var) %||% seq_len(ncol_)
  pred[[columns_to]] = rep(colnames_, each = nrow_)

  # then, flatten the first two dimensions of the prediction variable
  dims = dim(var)
  dim(var) = c(prod(dims[c(1,2)]), dims[-c(1:2)])
  pred[[varname]] = var

  pred
}
