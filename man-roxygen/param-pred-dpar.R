#' @param dpar For `add_epred_<%=pred_type%>()` and `add_linpred_<%=pred_type%>()`: Should distributional regression
#' parameters be included in the output? Valid only for models that support distributional regression parameters,
#' such as submodels for variance parameters (as in `brms::brm()`). If `TRUE`, distributional regression
#' parameters are included in the output as additional columns named after each parameter
#' (alternative names can be provided using a list or named vector, e.g. `c(sigma.hat = "sigma")`
#' would output the `"sigma"` parameter from a model as a column named `"sigma.hat"`).
#' If `NULL` or `FALSE` (the default), distributional regression parameters are not included.
