# Documentation of supported models
#
# Author: Matthew Kay
###############################################################################


#' Models supported by tidybayes
#'
#' @name tidybayes-models
#'
#' @description
#'
#' Tidybayes supports two classes of models and sample formats: Models/formats that provide prediction functions, and those that
#' do not.
#'
#' @section All Supported Models/Sample Formats:
#'
#' **All supported models/formats** support the base tidybayes sample extraction functions, such as
#' [tidy_draws()], [spread_draws()], [gather_draws()], [spread_rvars()], and [gather_rvars()].
#' These models/formats include:
#'
#' \itemize{
#'   \item [rstan][rstan::stan] models
#'   \item [cmdstanr](https://mc-stan.org/cmdstanr/) models
#'   \item [brms::brm()] models
#'   \item [rstanarm][rstanarm::rstanarm-package] models
#'   \item [runjags::runjags()] models
#'   \item [rjags::jags.model()] models, if sampled using [rjags::coda.samples()]
#'   \item [jagsUI::jags()] models
#'   \item [MCMCglmm::MCMCglmm()] models
#'   \item [coda::mcmc()] and [coda::mcmc.list()] objects, which are output by several model
#'     types.
#'   \item [posterior::draws] objects
#'   \item Any object with an implementation of [posterior::as_draws_df()] or [posterior::as_draws()].
#'     For a list of those available in your environment, run `methods(as_draws_df)` or `methods(as_draws)`
#'   \item Any object with an implementation of [coda::as.mcmc.list()].
#'     For a list of those available in your environment, run `methods(as.mcmc.list)`
#' }
#'
#' If you install the [tidybayes.rethinking](https://mjskay.github.io/tidybayes.rethinking/) package, models from
#' the [rethinking](https://github.com/rmcelreath/rethinking) package are also supported.
#'
#'
#' @section Models Supporting Prediction:
#'
#' In addition, the **following models support fit and prediction** extraction functions, such as
#' [add_epred_draws()], [add_predicted_draws()], [add_linpred_draws()], [add_epred_rvars()],
#' [add_predicted_rvars()], and [add_linpred_rvars()]:
#'
#' \itemize{
#'   \item [brms::brm()] models
#'   \item [rstanarm][rstanarm::rstanarm-package] models
#'   \item any package with implementations of [rstantools::posterior_epred()],
#'     [rstantools::posterior_predict()], or [rstantools::posterior_linpred()] that
#'     include an argument called `newdata` which takes a data frame of predictors.
#' }
#'
#' **If your model type is not in the above list**, you may still be able to use the [add_draws()]
#' function to turn matrices of predictive draws (or fit draws) into tidy data frames. Or,
#' you can wrap output from a prediction function in `posterior::rvar()` and add it
#' to a data frame so long as that output is a matrix with draws as rows.
#'
#' If you install the [tidybayes.rethinking](https://mjskay.github.io/tidybayes.rethinking/) package, models from
#' the [rethinking](https://github.com/rmcelreath/rethinking) package are also supported.
#'
#'
#' @section Extending tidybayes:
#'
#' To include basic support for new models, one need only implement the [tidy_draws()] generic function
#' for that model. Alternatively, objects that support [posterior::as_draws()] or [coda::as.mcmc.list()]
#' will automatically be supported by [tidy_draws()].
#'
#' To include support for estimation and prediction, one must either implement the [epred_draws()],
#' [predicted_draws()], and [linpred_draws()] functions or their correspond functions from
#' \pkg{rstantools}: [rstantools::posterior_epred()], [rstantools::posterior_predict()], and
#' [rstantools::posterior_linpred()]. If you take the latter approach, you should include
#' `newdata` and `ndraws` arguments that work as documented in `predicted_draws()`.
#'
NULL
