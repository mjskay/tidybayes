# spread_rvars
#
# Author: mjskay
###############################################################################


# spread_rvars ------------------------------------------------------------

#' Extract random variables from a Bayesian model into a tidy data format
#'
#' Extract draws from a Bayesian model for one or more variables (possibly with named
#' dimensions) into one of two types of long-format data frames containing [posterior::rvar] objects.
#'
#' @param model A supported Bayesian model fit. Tidybayes supports a variety of model objects;
#' for a full list of supported models, see [tidybayes-models].
#' @param ... Expressions in the form of
#' `variable_name[dimension_1, dimension_2, ...]`. See *Details*.
#' @param n The number of draws to return, or `NULL` to return all draws.
#' @param seed A seed to use when subsampling draws (i.e. when `n` is not `NULL`).
#' @return A data frame.
#' @author Matthew Kay
#' @seealso [spread_draws()], [recover_types()], [compose_data()].
#' @keywords manip
#'
#' @importFrom purrr reduce
#' @importFrom rlang enquos
#' @importFrom dplyr inner_join
#' @export
spread_rvars = function(model, ..., n = NULL, seed = NULL) {
  draws = sample_draws_from_rvars_(model, n, seed)

  list_of_rvar_tibbles = lapply(enquos(...), function(variable_spec) {
    spread_rvars_(draws, variable_spec)
  })

  out = reduce(list_of_rvar_tibbles, function(tibble1, tibble2) {
    by_ = intersect(names(tibble1), names(tibble2))
    inner_join(tibble1, tibble2, by = by_)
  })

  out[[".stub"]] = NULL

  out
}


spread_rvars_ = function(draws, variable_spec) {
  spec = parse_variable_spec(variable_spec)
  variable_names = spec[[1]]
  dimension_names = spec[[2]]
  wide_dimension_name = spec[[3]]

  if (!is.null(wide_dimension_name)) {
    # TODO: implement
    stop("spread_rvars does not currently support wide dimensions (`|` syntax).")
  }

  #for specs like `x`, replace with equivalent of `x[]`
  if (length(dimension_names) == 0) dimension_names = ""

  #ensure equal dimensions in all variables
  var1 = draws[[variable_names[[1]]]]
  var1_dim = dim(var1)
  for (variable_name in variable_names) {
    var = draws[[variable_name]]
    if (is.null(var)) {
      stop(
        "The variable `", variable_name, "` was not found in the model."
      )
    } else if (!all(dim(var) == var1_dim)) {
      stop(
        "All variables under the same subscript must have the same shape.\n",
        "`", variable_name, "` has shape [", paste0(dim(var), collapse = ","), "] but\n",
        "`", variable_names[[1]], "` has shape [", paste0(var1_dim, collapse = ","), "]."
      )
    }
  }

  #ensure variables have same number of dimensions as requested (since we
  #already checked they are all equal we only have to check one here)
  if (length(var1_dim) != length(dimension_names)) {
    stop("`", variable_names[[1]], "` has ", length(var1_dim), " dimensions, not ", length(dimension_names))
  }

  # determine which indices we are flattening and which we aren't
  empty_dimensions = dimension_names == ""
  keep_dimensions = which(empty_dimensions)
  flatten_dimensions = which(!empty_dimensions)

  # determine a permutation to bring flattened dimensions to the front and
  # the final dims the flattened variables should have
  dim_perm = c(flatten_dimensions, keep_dimensions)
  flattened_dim = c(prod(var1_dim[flatten_dimensions]), var1_dim[keep_dimensions])

  # construct basic data frame with all combinations of the flattened dimensions
  # as columns, which we can then add flattened versions of the variables onto
  # start with numeric names
  dimname_lists = lapply(dim(var)[flatten_dimensions], seq_len)
  .dimnames = dimnames(var)[flatten_dimensions]
  if (!is.null(.dimnames)) {
    # where character names are provided, use those instead of the numeric names
    dimname_lists = lapply(seq_along(dimname_lists), function(i) .dimnames[[i]] %||% dimname_lists[[i]])
  }
  # expand out the dimname lists into the full set of combinations
  # keep this as a list for now in case there are no dimensions
  out = as.list(expand.grid(dimname_lists, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE))
  names(out) = dimension_names[flatten_dimensions]

  # stub column for joins involving variables with no indices
  out[[".stub"]] = 1L

  # move all requested dimensions in each variable to the front and flatten
  for (variable_name in variable_names) {
    var = draws[[variable_name]]

    if (length(flatten_dimensions) == 0 && length(dim(var)) == 1 && length(var) > 1) {
      # this is a spec like x[] for a vector (non-scalar) variable with 1 dimension.
      # in this case leaving that dimension off should cause that dim
      # to be columns, not rows in the output --- so we have to transpose
      var = t(var)
    }
    if (length(flatten_dimensions) > 0) {
      # move requested dimensions to the front and flatten
      var = aperm(var, dim_perm)
      dim(var) = flattened_dim
    }

    out[[variable_name]] = var
  }

  #convert dimensions back into usable data types
  out = convert_cols_to_types_from_model(out, dimension_names, draws)

  as_tibble(out)
}


# helpers -----------------------------------------------------------------

# sample draws from a draws_rvars
#' @importFrom posterior as_draws_rvars resample_draws
sample_draws_from_rvars_ = function(model, n = NULL, seed = NULL) {
  draws = as_draws_rvars(model)

  if (!is.null(n)) {
    if (!is.null(seed)) set.seed(seed)
    draws = resample_draws(draws, ndraws = n)
  }

  draws
}

