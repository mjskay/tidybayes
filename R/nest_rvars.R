# [un]nest_rvars
#
# Author: mjskay
###############################################################################

#' Nest and unnest `rvar` columns in data frames
#'
#' Converts between data-frame-of-`rvar`s format and long-data-frame-of-draws
#' formats by nesting or unnesting all columns containing [`posterior::rvar`]
#' objects.
#'
#' @param data A data frame to nest or unnest.
#'  - For `nest_rvars()`, the data frame should be in long-data-frame-of-draws
#'    format; i.e. it should contain a `.draw` column (and optionally `.chain`
#'    and `.iteration` columns) indexing draws. It should be a grouped by any
#'    columns that are not intended to be nested.
#'  - For `unnest_rvars()`, the data frame should have at least one column that
#'    is an [`rvar`]; all `rvar` columns will be unnested.
#'
#' @return
#'
#' For `nest_rvars()`, returns a data frame without `.chain`, `.iteration`, and
#' `.draw` columns, where all non-grouped columns have been converted to [`rvar`]s.
#'
#' For `unnest_rvars()`, returns a data frame with `.chain`, `.iteration`, and
#' `.draw` columns added, where every [`rvar`] column in the input has been
#' converted to (one or more) columns containing draws from those [`rvar`]s
#' in long format. The result is grouped by all non-[`rvar`] columns in the
#' input; this ensures that `nest_rvars(unnest_rvars(x))` returns `x`.
#'
#' @examples
#'
#' library(dplyr)
#'
#' data(RankCorr, package = "ggdist")
#'
#' # here's a data frame with some rvars
#' rvar_df = RankCorr %>%
#'   spread_rvars(b[i,], tau[i])
#' rvar_df
#'
#' # we can unnest it into long format.
#' # note how the result is grouped by all non-rvar input columns,
#' # and nested indices in `b` are converted into columns.
#' draws_df = rvar_df %>%
#'   unnest_rvars()
#' draws_df
#'
#' # calling nest_rvars() again on the result of unnest_rvars()
#' # recovers the original data frame
#' nest_rvars(draws_df)
#'
#' @name nest_rvars
#' @importFrom dplyr group_modify
#' @export
nest_rvars = function(data) {
  group_modify(data, function(d, keys) {
    if (all(is.na(d[[".chain"]])) || all(is.na(d[[".iteration"]]))) {
      # if either of chain or iteration information is missing, we must treat
      # both as missing (since we can't use one without the other)
      d[[".chain"]] = 1
      d[[".iteration"]] = d[[".draw"]]
    }
    as_tibble(as_draws_rvars(as_draws_df(d)))
  })
}

#' @rdname nest_rvars
#' @importFrom posterior is_rvar
#' @export
unnest_rvars = function(data) {
  rvar_cols = map_lgl_(data, is_rvar)
  groups_ = names(data)[!rvar_cols]

  out = as_tibble(map_dfr_(vctrs::vec_chop(data), function(d) {
    constants = d[!rvar_cols]
    rvars = as.list(d[rvar_cols])
    draws_df = as_draws_df(as_draws_rvars(rvars))
    # convert from draws_df to plain data.frame to avoid
    # warning about meta-data being dropped
    class(draws_df) = "data.frame"
    # convert from tibble to plain data.frame to fix
    # incorrect binding in cbind() in R < 4
    class(constants) = "data.frame"
    cbind(constants, draws_df)
  }))

  group_by_at(out, groups_)
}
