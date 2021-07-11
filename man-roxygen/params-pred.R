#' @template param-pred-newdata
#' @template param-pred-object
#' @param epred The name of the output column for `add_epred_rvars()`; default `".epred"`.
#' @param prediction The name of the output column for `add_predicted_rvars()`; default `".prediction"`.
#' @param linpred The name of the output column for `add_linpred_rvars()`; default `".linpred"`.
#' @param ... Additional arguments passed to the underlying prediction method for the type of
#' model given.
#' @template param-ndraws
#' @template param-seed
#' @param re_formula formula containing group-level effects to be considered in the prediction.
#' If `NULL` (default), include all group-level effects; if `NA`, include no group-level effects.
#' Some model types (such as [brms::brmsfit] and [rstanarm::stanreg-objects]) allow
#' marginalizing over grouping factors by specifying new levels of a factor in `newdata`. In the case of
#' [brms::brm()], you must also pass `allow_new_levels = TRUE` here to include new levels (see
#' [brms::posterior_predict()]).
#' @param dpar For `add_epred_rvars()`: Should distributional regression
#' parameters be included in the output? Valid only for models that support distributional regression parameters,
#' such as submodels for variance parameters (as in `brms::brm()`). If `TRUE`, distributional regression
#' parameters are included in the output as additional columns named after each parameter
#' (alternative names can be provided using a list or named vector, e.g. `c(sigma.hat = "sigma")`
#' would output the `"sigma"` parameter from a model as a column named `"sigma.hat"`).
#' If `NULL` or `FALSE` (the default), distributional regression parameters are not included.
