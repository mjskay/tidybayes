# tidy_draws
#
# Author: mjskay
###############################################################################


#' Get a sample of posterior draws from a model as a tibble
#'
#' Extract draws from a Bayesian fit into a wide-format data frame with a
#' `.chain`, `.iteration`, and `.draw` column, as well as all variables
#' as columns. This function does not parse indices from variable names
#' (e.g. for variable names like `"x[1]"`); see [spread_draws()] or
#' [gather_draws()] for functions that parse variable indices.
#'
#' This function can be useful for quick glances at models (especially
#' combined with [gather_variables()] and [median_qi()]), and for models with
#' parameters without indices in their names (like `"x[1]"`).
#' [spread_draws()] and [gather_draws()], which *do* parse variable name indices,
#' call this function internally if their input is not already a tidy data frame.
#'
#' To provide support for new models in tidybayes,
#' you must provide an implementation of this function *or* an implementation
#' of [coda::as.mcmc.list()] (`tidy_draws` should work on any model
#' with an implementation of [coda::as.mcmc.list()])
#'
#' `tidy_draws()` can be applied to a data frame that is already a tidy-format data frame
#' of draws, provided it has one row per draw. In other words, it can be applied to data frames
#' that have the same format it returns, and it will return the same data frame back, while
#' checking to ensure the `.chain`, `.iteration`, and `.draw` columns are all
#' integers (converting if possible) and that the `.draw` column is unique. This allows
#' you to pass already-tidy-format data frames into other tidybayes functions, like
#' [spread_draws()] or [gather_draws()]. This functionality can be useful if the
#' tidying step is expensive: you can tidy once, possibly subsetting to some
#' particular variables of interest, then call [spread_draws()] or [gather_draws()]
#' repeatedly to extract variables and indices from the already-tidied data frame.
#'
#' @template param-model
#' @param ... Further arguments passed to other methods (mostly unused).
#' @return A data frame (actually, a [tibble][tibble::tibble]) with a `.chain` column,
#' `.iteration` column, `.draw` column, and one column for every variable in `model`.
#' @author Matthew Kay
#' @seealso [spread_draws()] or [gather_draws()], which use this function
#' internally and provides a friendly interface for extracting tidy data frames from model fits.
#' @keywords manip
#' @examples
#'
#' library(magrittr)
#'
#' data(line, package = "coda")
#'
#' line %>%
#'   tidy_draws()
#'
#' @importFrom dplyr bind_cols
#' @importFrom tibble as_tibble tibble
#' @importFrom coda as.mcmc.list as.mcmc
#' @export
tidy_draws = function(model, ...) UseMethod("tidy_draws")

#' @rdname tidy_draws
#' @export
#' @importFrom posterior as_draws_df as_draws
#' @importFrom coda as.mcmc.list
tidy_draws.default = function(model, ...) {
  signature = class(model)
  if (has_method("as_draws_df", signature)) {
    draws = as_tibble(as_draws_df(model))
  } else if (has_method("as_draws", signature)) {
    draws = as_tibble(as_draws_df(as_draws(model)))
  } else if (has_method("as.mcmc.list", signature)) {
    draws = tidy_draws(as.mcmc.list(model))
  } else {
    stop0(
      "Don't know how to convert an object of class ", deparse0(signature), "\n",
      "into a tidy data frame of draws. See help(\"tidybayes-models\") for a list\n",
      "of models supported by tidy_draws()."
    )
  }
  attr(draws, "tidybayes_constructors") = attr(model, "tidybayes_constructors")
  draws
}

#' @rdname tidy_draws
#' @export
tidy_draws.draws = function(model, ...) {
  draws = as_tibble(as_draws_df(model))
  attr(draws, "tidybayes_constructors") = attr(model, "tidybayes_constructors")
  draws
}

#' @rdname tidy_draws
#' @export
tidy_draws.data.frame = function(model, ...) {
  stop_message = paste0(
    "To use a data frame directly with `tidy_draws()`, it must already be a\n",
    "  tidy-format data frame of draws: it must have integer-like `.chain`\n",
    "  `.iteration`, and `.draw` columns with one row per draw.\n",
    "\n"
  )

  # keep the constructors around in case they were set on the original model
  constructors = attr(model, "tidybayes_constructors")

  # iterate over draw index columns to check they are integers, recording if they passed the check
  # (and cleaning as necessary)
  check_cols = c(".chain", ".iteration", ".draw")
  passed = sapply(check_cols, function(col) {
    col_value = model[[col]]

    if (is.null(col_value)) return(FALSE)
    if (is.integer(col_value)) return(TRUE)
    if (is.logical(col_value) || is_integerish(col_value)) {
      model[[col]] <<- as.integer(col_value)
      return(TRUE)
    }

    # if we make it this far, the column did not pass all checks
    FALSE
  })

  failed_cols = check_cols[!passed]
  if (length(failed_cols) > 0) {
    stop0(stop_message,
      "  The following columns are not integer-like (or are missing):\n",
      "    ", paste0("`", failed_cols, "`", collapse = ", ")
    )
  }

  if (length(unique(model[[".draw"]])) != nrow(model)) {
    stop0(stop_message,
      "  The `.draw` column in the input data frame has more than one row per draw\n",
      "  (its values are not unique)."
    )
  }

  attr(model, "tidybayes_constructors") = constructors
  model
}

#' @rdname tidy_draws
#' @importFrom tibble add_column
#' @export
tidy_draws.mcmc.list = function(model, ...) {
  draws = do.call(rbind, lapply(seq_along(model), function(chain) {
    n = nrow(model[[chain]])
    iteration = seq_len(n)

    add_column(
      as_tibble(
        as.matrix(model[[chain]]),
        .name_repair = "minimal"
      ),

      .chain = chain,
      .iteration = iteration,
      .draw = draw_from_chain_and_iteration_(chain, iteration),

      .before = 1
    )
  }))

  attr(draws, "tidybayes_constructors") = attr(model, "tidybayes_constructors")
  draws
}

#' @rdname tidy_draws
#' @export
tidy_draws.stanfit = function(model, ...) {
  draws = as_tibble(as_draws_df(model))
  draws = select(draws, all_of(c(".chain", ".iteration", ".draw")), everything())
  draws = add_rstan_sampler_param_draws(draws, model)

  attr(draws, "tidybayes_constructors") = attr(model, "tidybayes_constructors")
  draws
}

#' @rdname tidy_draws
#' @export
tidy_draws.stanreg = function(model, ...) {
  #stanreg objects have more info provided for variable names than the underlying stanfit,
  #so we dont' just do tidy_draws(model$stanfit)
  sample_matrix = as.array(model) #[iteration, chain, variable]
  n_chain = dim(sample_matrix)[[2]]
  mcmc_list = as.mcmc.list(lapply(seq_len(n_chain), function(chain) as.mcmc(sample_matrix[, chain, ]))) # nolint
  draws = tidy_draws(mcmc_list, ...)
  draws = add_rstan_sampler_param_draws(draws, model$stanfit)

  attr(draws, "tidybayes_constructors") = attr(model, "tidybayes_constructors")
  draws
}

#' Helper to add sampler params from an rstan model to a tidy data frame of draws
#' so long as sampler params are actually present
#' @noRd
add_rstan_sampler_param_draws = function(draws, model, inc_warmup = FALSE) {
  # TODO: this is ugly but I can't find a better way to determine if a model
  # type supports sampler params or not
  sampler_params = tryCatch({
    rstan::get_sampler_params(model, inc_warmup = inc_warmup)
  }, error = function(e) {
    list()
  })

  sampler_param_draws = map_dfr_(sampler_params, as.data.frame, optional = TRUE)

  if (length(sampler_param_draws) > 0) {
    bind_cols(draws, sampler_param_draws)
  } else {
    draws
  }
}

#' @rdname tidy_draws
#' @export
tidy_draws.runjags = function(model, ...) {
  draws = tidy_draws(as.mcmc.list(model), ...)
  attr(draws, "tidybayes_constructors") = attr(model, "tidybayes_constructors")
  draws
}

#' @rdname tidy_draws
#' @export
tidy_draws.jagsUI = function(model, ...) {
  draws = tidy_draws(as.mcmc.list(model$samples), ...)
  attr(draws, "tidybayes_constructors") = attr(model, "tidybayes_constructors")
  draws
}

#' @rdname tidy_draws
#' @importFrom dplyr everything
#' @export
tidy_draws.brmsfit = function(model, ...) {
  draws = tidy_draws(model$fit)

  attr(draws, "tidybayes_constructors") = attr(model, "tidybayes_constructors")
  draws
}

#' @rdname tidy_draws
#' @importFrom posterior as_draws_df
#' @export
tidy_draws.CmdStanFit = function(model, ...) {
  draws = as_tibble(as_draws_df(model$draws()))

  attr(draws, "tidybayes_constructors") = attr(model, "tidybayes_constructors")
  draws
}

#' @rdname tidy_draws
#' @importFrom posterior bind_draws as_draws_df
#' @export
tidy_draws.CmdStanMCMC = function(model, ...) {
  draws = as_tibble(as_draws_df(bind_draws(model$draws(), model$sampler_diagnostics())))

  attr(draws, "tidybayes_constructors") = attr(model, "tidybayes_constructors")
  draws
}

#' @rdname tidy_draws
#' @export
tidy_draws.matrix = function(model, ...) {
  # assume matrix indexed by [iterations, variables]
  draws = tidy_draws(as.mcmc.list(as.mcmc(model)), ...)

  attr(draws, "tidybayes_constructors") = attr(model, "tidybayes_constructors")
  draws
}

#' @rdname tidy_draws
#' @importFrom dplyr inner_join
#' @export
tidy_draws.MCMCglmm = function(model, ...) {
  # draws from MME solutions, including fixed effects
  sol_draws = tidy_draws(model$Sol, ...)

  # draws from (co)variance matrices, ordinal cutpoints, and latent variables
  other_draws =
    model[c("VCV", "CP", "Liab")] %>%
    discard_(is.null) %>%
    map2_(names(.), function(draws, param_type) {
      dimnames(draws)[[2]] %<>% paste0(param_type, "_", .)
      tidy_draws(draws, ...)
    })

  draws = sol_draws %>%
    list() %>%
    c(other_draws) %>%
    reduce_(inner_join, by = c(".chain", ".iteration", ".draw"))

  attr(draws, "tidybayes_constructors") = attr(model, "tidybayes_constructors")
  draws
}
