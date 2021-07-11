# summarise_draws.grouped_df
#
# Author: mjskay
###############################################################################

#' @importFrom posterior summarise_draws
#' @export
posterior::summarise_draws

#' Summaries of draws in `grouped_df` objects
#'
#' An implemention of `posterior::summarise_draws()` for grouped data frames
#' (`dplyr::grouped_df` objects) such as returned by `dplyr::group_by()` and
#' the various grouped-data-aware functions in tidybayes, such as `spread_draws()`,
#' `gather_draws()`, `add_epred_draws()`, and `add_predicted_draws()`.
#' This function provides a quick way to get a variety of summary statistics
#' and diagnostics on draws.
#'
#' While `posterior::summarise_draws()` can operate on tidy data frames of
#' draws in the `posterior::draws_df()` format, that format does not support
#' grouping columns. This provides an implementation of `summarise_draws()`
#' that does support grouped data tables, essentially applying
#' `posterior::summarise_draws()` to every sub-table of `.x` implied by the
#' groups defined on the data frame.
#'
#' See `posterior::summarise_draws()` for
#' more details on the summary statistics and diagnostics you can use with this
#' function. If you just want point summaries and intervals (not diagnostics),
#' particularly for plotting, see `point_interval()`, which returns long-format
#' data tables more suitable for that purpose (especially if you want to plot
#' multiple uncertainty levels).
#'
#' @inheritParams posterior::summarise_draws
#' @param .x A grouped data frame (`dplyr::grouped_df` object) such as returned by
#' `dplyr::group_by()` where the data frame in each group (ignoring grouping columns)
#' has the structure of a `posterior::draws_df()` object: `".chain"`, `".iteration"`, and
#' `".draw"` columns, with the remaining (non-grouping) columns being draws
#' from variables.
#'
#' @return A data frame (actually, a [tibble][tibble::tibble]) with all grouping
#' columns from `.x`, a `"variable"` column containing variable names from `.x`,
#' and the remaining columns containing summary statistics and diagnostics.
#' @author Matthew Kay
#' @seealso [posterior::summarise_draws()], [point_interval()]
#' @keywords manip
#' @examples
#'
#' library(posterior)
#' library(dplyr)
#'
#' d = posterior::example_draws()
#'
#' # The default posterior::summarise_draws() summarises all variables without
#' # splitting out indices:
#' summarise_draws(d)
#'
#' # The grouped_df implementation of summarise_draws() in tidybayes can handle
#' # output from spread_draws(), which is a grouped data table with the indices
#' # (here, `i`) left as columns:
#' d %>%
#'   spread_draws(theta[i]) %>%
#'   summarise_draws()
#'
#' # Summary functions can also be provided, as in posterior::summarise_draws():
#' d %>%
#'   spread_draws(theta[i]) %>%
#'   summarise_draws(median, mad, rhat, ess_tail)
#'
#' @importFrom posterior as_draws_df
#' @importFrom dplyr group_modify
#' @export
summarise_draws.grouped_df = function(.x, ...) {
  group_modify(.x, function(.x, keys) {
    summarise_draws(as_draws_df(.x), ...)
  })
}
