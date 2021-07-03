# [un]nest_rvars
#
# Author: mjskay
###############################################################################

#' @importFrom posterior is_rvar
unnest_rvars = function(data) {
  rvar_cols = map_lgl_(data, is_rvar)

  as_tibble(map_dfr_(vctrs::vec_chop(data), function(d) {
    constants = d[!rvar_cols]
    rvars = as.list(d[rvar_cols])
    cbind(constants, as.data.frame(as_draws_df(as_draws_rvars(rvars)), optional = TRUE))
  }))
}
