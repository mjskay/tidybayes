#' @param draw_indices Character vector of column names that should be treated
#' as indices of draws. Operations are done within combinations of these values.
#' The default is `c(".chain", ".iteration", ".draw")`, which is the same names
#' used for chain, iteration, and draw indices returned by [tidy_draws()].
#' Names in `draw_indices` that are not found in the data are ignored.
