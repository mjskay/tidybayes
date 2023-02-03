#' @details
#'
#' Consider a model like:
#'
#' \deqn{\begin{aligned}
#' y &\sim \textrm{SomeDist}(\theta_1, \theta_2)\\
#' f_1(\theta_1) &= \alpha_1 + \beta_1 x\\
#' f_2(\theta_2) &= \alpha_2 + \beta_2 x
#' \end{aligned}}{
#' y ~ SomeDist(\theta_1, \theta_2),
#' f_1(\theta_1) = \alpha_1 + \beta_1*x,
#' f_2(\theta_2) = \alpha_2 + \beta_2*x
#' }
#'
#' This model has:
#'
#' - an outcome variable, \eqn{y}
#' - a response distribution, \eqn{\textrm{SomeDist}}{SomeDist}, having parameters \eqn{\theta_1}
#' (with link function \eqn{f_1}) and \eqn{\theta_2} (with link function \eqn{f_2})
#' - a single predictor, \eqn{x}
#' - coefficients \eqn{\alpha_1}, \eqn{\beta_1}, \eqn{\alpha_2}, and \eqn{\beta_2}
#'
#' We fit this model to some observed data, \eqn{y_\textrm{obs}}{y_obs}, and predictors,
#' \eqn{x_\textrm{obs}}{x_obs}. Given new values of predictors, \eqn{x_\textrm{new}}{x_new},
#' supplied in the data frame `newdata`, the functions for posterior draws are
#' defined as follows:
#'
#' - `add_predicted_<%=pred_type%>()` adds <%=draws%> from the **posterior predictive distribution**,
#' \eqn{p(y_\textrm{new} | x_\textrm{new}, y_\textrm{obs})}{p(y_new | x_new, y_obs)},
#' to the data.
#' It corresponds to [rstanarm::posterior_predict()] or [brms::posterior_predict()].
#'
#' - `add_epred_<%=pred_type%>()` adds <%=draws%> from the **expectation of the posterior predictive
#' distribution**, aka the conditional expectation,
#' \eqn{E(y_\textrm{new} | x_\textrm{new}, y_\textrm{obs})}{E(y_new | x_new, y_obs)},
#' to the data.
#' It corresponds to [rstanarm::posterior_epred()] or [brms::posterior_epred()].
#' Not all models support this function.
#'
#' - `add_linpred_<%=pred_type%>()` adds <%=draws%> from the **posterior linear predictors** to the data.
#' It corresponds to [rstanarm::posterior_linpred()] or [brms::posterior_linpred()].
#' Depending on the model type and additional parameters passed, this may be:
#'
#'   - The untransformed linear predictor, e.g.
#'     \eqn{p(f_1(\theta_1) | x_\textrm{new}, y_\textrm{obs})}{p(f_1(\theta_1) | x_new, y_obs)} =
#'     \eqn{p(\alpha_1 + \beta_1 x_\textrm{new} | x_\textrm{new}, y_\textrm{obs})}{p(\alpha_1 + \beta_1*x_new | x_new, y_obs)}.
#'     This is returned by `add_linpred_<%=pred_type%>(transform = FALSE)` for \pkg{brms} and \pkg{rstanarm} models.
#'     It is analogous to `type = "link"` in [predict.glm()].
#'   - The inverse-link transformed linear predictor, e.g.
#'     \eqn{p(\theta_1 | x_\textrm{new}, y_\textrm{obs})}{p(\theta_1 | x_new, y_obs)} =
#'     \eqn{p(f_1^{-1}(\alpha_1 + \beta_1 x_\textrm{new}) | x_\textrm{new}, y_\textrm{obs})}{p(f_1^-1(\alpha_1 + \beta_1*x_new) | x_new, y_obs)}.
#'     This is returned by `add_linpred_<%=pred_type%>(transform = TRUE)` for \pkg{brms} and \pkg{rstanarm} models.
#'     It is analogous to `type = "response"` in [predict.glm()].
#'
#'   **NOTE:** `add_linpred_<%=pred_type%>(transform = TRUE)` and `add_epred_<%=pred_type%>()` may be equivalent but
#'   are not guaranteed to be. They are equivalent when the expectation of the response
#'   distribution is equal to its first parameter, i.e. when \eqn{E(y) = \theta_1}. Many
#'   distributions have this property (e.g. Normal distributions, Bernoulli distributions),
#'   but not all. If you want the expectation of the posterior predictive, it is best to
#'   use `add_epred_<%=pred_type%>()` if available, and if not available, verify this property holds prior
#'   to using `add_linpred_<%=pred_type%>()`.
#'
#' <%if(pred_type == "draws") {%>
#' - `add_residual_<%=pred_type%>()` adds <%=draws%> from residuals,
#' \eqn{p(y_\textrm{obs} - y_\textrm{new} | x_\textrm{new}, y_\textrm{obs})}{p(y_obs - y_new | x_new, y_obs)},
#' to the data.
#' It corresponds to [brms::residuals.brmsfit()].
#' <%}%>
#'
#' The corresponding functions without `add_` as a prefix are alternate spellings
#' with the opposite order of the first two arguments: e.g. `add_predicted_<%=pred_type%>(newdata, object)`
#' versus `predicted_<%=pred_type%>(object, newdata)`. This facilitates use in data
#' processing pipelines that start either with a data frame or a model.
#'
#' Given equal choice between the two, the spellings prefixed with `add_`
#' are preferred.
