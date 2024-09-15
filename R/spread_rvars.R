# spread_rvars
#
# Author: mjskay
###############################################################################


# spread_rvars ------------------------------------------------------------

#' Extract draws from a Bayesian model into tidy data frames of random variables
#'
#' Extract draws from a Bayesian model for one or more variables (possibly with named
#' dimensions) into one of two types of long-format data frames of [posterior::rvar] objects.
#'
#' Imagine a JAGS or Stan fit named `model`. The model may contain a variable named
#' `b[i,v]` (in the JAGS or Stan language) with dimension `i` in `1:100` and
#' dimension `v` in `1:3`. However, the default format for draws returned from
#' JAGS or Stan in R will not reflect this indexing structure, instead
#' they will have multiple columns with names like `"b[1,1]"`, `"b[2,1]"`, etc.
#'
#' `spread_rvars` and `gather_rvars` provide a straightforward
#' syntax to translate these columns back into properly-indexed [`rvar`][posterior::rvar]s in two different
#' tidy data frame formats, optionally recovering dimension types (e.g. factor levels) as it does so.
#'
#' `spread_rvars` will spread names of variables in the model across the data frame as column names,
#' whereas `gather_rvars` will gather variable names into a single column named `".variable"` and place
#' values of variables into a column named `".value"`. To use naming schemes from other packages
#' (such as `broom`), consider passing
#' results through functions like [to_broom_names()] or [to_ggmcmc_names()].
#'
#' For example, `spread_rvars(model, a[i], b[i,v])` might return a data frame with:
#' \itemize{
#'    \item column `"i"`: value in `1:5`
#'    \item column `"v"`: value in `1:10`
#'    \item column `"a"`: [`rvar`][posterior::rvar] containing draws from `"a[i]"`
#'    \item column `"b"`: [`rvar`][posterior::rvar] containing draws from `"b[i,v]"`
#'  }
#'
#' `gather_rvars(model, a[i], b[i,v])` on the same model would return a data frame with:
#' \itemize{
#'    \item column `"i"`: value in `1:5`
#'    \item column `"v"`: value in `1:10`, or `NA`
#'      on rows where `".variable"` is `"a"`.
#'    \item column `".variable"`: value in `c("a", "b")`.
#'    \item column `".value"`: [`rvar`][posterior::rvar] containing draws from `"a[i]"` (when `".variable"` is `"a"`)
#'      or `"b[i,v]"` (when `".variable"` is `"b"`)
#'  }
#'
#' `spread_rvars` and `gather_rvars` can use type information
#' applied to the `model` object by [recover_types()] to convert columns
#' back into their original types. This is particularly helpful if some of the dimensions in
#' your model were originally factors. For example, if the `v` dimension
#' in the original data frame `data` was a factor with levels `c("a","b","c")`,
#' then we could use `recover_types` before `spread_rvars`:
#'
#' \preformatted{model \%>\%
#'  recover_types(data) %\>\%
#'  spread_rvars(model, b[i,v])
#' }
#'
#' Which would return the same data frame as above, except the `"v"` column
#' would be a value in `c("a","b","c")` instead of `1:3`.
#'
#' For variables that do not share the same subscripts (or share
#' some but not all subscripts), we can supply their specifications separately.
#' For example, if we have a variable `d[i]` with the same `i` subscript
#' as `b[i,v]`, and a variable `x` with no subscripts, we could do this:
#'
#' \preformatted{spread_rvars(model, x, d[i], b[i,v])}
#'
#' Which is roughly equivalent to this:
#'
#' \preformatted{spread_rvars(model, x) \%>\%
#'  inner_join(spread_rvars(model, d[i])) \%>\%
#'  inner_join(spread_rvars(model, b[i,v]))
#' }
#'
#' Similarly, this:
#'
#' \preformatted{gather_rvars(model, x, d[i], b[i,v])}
#'
#' Is roughly equivalent to this:
#'
#' \preformatted{bind_rows(
#'  gather_rvars(model, x),
#'  gather_rvars(model, d[i]),
#'  gather_rvars(model, b[i,v])
#' )}
#'
#' The `c` and `cbind` functions can be used to combine multiple variable names that have
#' the same dimensions. For example, if we have several variables with the same
#' subscripts `i` and `v`, we could do either of these:
#'
#' \preformatted{spread_rvars(model, c(w, x, y, z)[i,v])}
#' \preformatted{spread_rvars(model, cbind(w, x, y, z)[i,v])  # equivalent}
#'
#' Each of which is roughly equivalent to this:
#'
#' \preformatted{spread_rvars(model, w[i,v], x[i,v], y[i,v], z[i,v])}
#'
#' Besides being more compact, the `c()`-style syntax is currently also slightly
#' faster (though that may change).
#'
#' Dimensions can be left nested in the resulting [`rvar`][posterior::rvar] objects by leaving their names
#' blank; e.g. `spread_rvars(model, b[i,])` will place the first index (`i`) into
#' rows of the data frame but leave the second index nested in the `b` column
#' (see *Examples* below).
#'
#' @template param-model
#' @param ... Expressions in the form of
#' `variable_name[dimension_1, dimension_2, ...]`. See *Details*.
#' @template param-ndraws
#' @template param-seed
#' @return A data frame.
#' @author Matthew Kay
#' @seealso [spread_draws()], [recover_types()], [compose_data()]. See also
#' [posterior::rvar()] and [posterior::as_draws_rvars()], the functions that power
#' `spread_rvars` and `gather_rvars`.
#' @keywords manip
#' @examples
#'
#' library(dplyr)
#'
#' data(RankCorr, package = "ggdist")
#'
#' RankCorr %>%
#'   spread_rvars(b[i, j])
#'
#' # leaving an index out nests the index in the column containing the rvar
#' RankCorr %>%
#'   spread_rvars(b[i, ])
#'
#' RankCorr %>%
#'   spread_rvars(b[i, j], tau[i], u_tau[i])
#'
#' # gather_rvars places variables and values in a longer format data frame
#' RankCorr %>%
#'   gather_rvars(b[i, j], tau[i], typical_r)
#'
#' @importFrom rlang enquos
#' @importFrom dplyr inner_join
#' @export
spread_rvars = function(model, ..., ndraws = NULL, seed = NULL) {
  draws = sample_draws_from_rvars_(model, ndraws, seed)

  list_of_rvar_tibbles = lapply(enquos(...), function(variable_spec) {
    spec = parse_variable_spec(variable_spec)
    spread_rvars_(draws, spec)
  })

  out = reduce_(list_of_rvar_tibbles, function(tibble1, tibble2) {
    by_ = intersect(names(tibble1), names(tibble2))
    inner_join(tibble1, tibble2, by = by_, multiple = "all")
  })

  out[[".stub"]] = NULL

  out
}


spread_rvars_ = function(draws, spec) {
  variable_names = spec[[1]]
  dimension_names = spec[[2]]
  wide_dimension_name = spec[[3]]

  if (!is.null(wide_dimension_name)) {
    stop0(
      "spread_rvars does not currently support wide dimensions (`|` syntax).\n",
      "Try leaving the index you would like to be wide blank instead."
    )
  }

  #for specs like `x`, treat as if user had input `x[]` (which is equivalent)
  if (length(dimension_names) == 0) dimension_names = ""

  #ensure equal dimensions in all variables
  var1 = draws[[variable_names[[1]]]]
  var1_dim = dim(var1)
  for (variable_name in variable_names) {
    var = draws[[variable_name]]
    if (is.null(var)) {
      stop0(
        "The variable `", variable_name, "` was not found in the model."
      )
    } else if (!all(dim(var) == var1_dim)) {
      stop0(
        "All variables under the same subscript must have the same shape.\n",
        "`", variable_name, "` has shape [", paste0(dim(var), collapse = ","), "] but\n",
        "`", variable_names[[1]], "` has shape [", paste0(var1_dim, collapse = ","), "]."
      )
    }
  }

  #ensure variables have same number of dimensions as requested (since we
  #already checked they are all equal we only have to check one here)
  if (length(var1_dim) != length(dimension_names)) {
    stop0("`", variable_names[[1]], "` has ", length(var1_dim), " dimensions, not ", length(dimension_names))
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
#' @importFrom posterior as_draws as_draws_rvars resample_draws ndraws nchains merge_chains
#' @importFrom stats weights
sample_draws_from_rvars_ = function(model, ndraws = NULL, seed = NULL) {
  signature = class(model)
  if (has_method("as_draws_rvars", signature)) {
    draws = as_draws_rvars(model)
  } else if (has_method("as_draws", signature)) {
    draws = as_draws_rvars(as_draws(model))
  } else {
    draws = as_draws_rvars(tidy_draws(model))
  }

  if (!is.null(ndraws)) {
    if (!is.null(seed)) set.seed(seed)
    # if there are multiple chains merge them here so resample_draws does
    # not give a message about merging chains
    if (nchains(draws) > 1) draws = merge_chains(draws)
    # if draws has no weights we must provide them or resample_draws returns
    # an error (weights are normalized by resample_draws so we don't have to)
    weights = weights(draws) %||% rep(1, ndraws(draws))
    draws = resample_draws(draws, ndraws = ndraws, weights = weights)
  }

  attr(draws, "tidybayes_constructors") = attr(model, "tidybayes_constructors")
  draws
}

