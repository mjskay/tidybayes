#' @param object A supported Bayesian model fit that can provide fits and predictions. Supported models
#' are listed in the second section of [tidybayes-models]: *Models Supporting Prediction*. While other
#' functions in this package (like [spread_<%=pred_type%>()]) support a wider range of models, to work with
#' `add_epred_<%=pred_type%>()`, `add_predicted_<%=pred_type%>()`, etc. a model must provide an interface for generating
#' predictions, thus more generic Bayesian modeling interfaces like `runjags` and `rstan` are not directly
#' supported for these functions (only wrappers around those languages that provide predictions, like `rstanarm`
#' and `brm`, are supported here).
