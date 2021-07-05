# [un]nest_rvars
#
# Author: mjskay
###############################################################################

#' @importFrom posterior is_rvar
unnest_rvars = function(data) {
  groups_ = group_vars(data)
  rvar_cols = map_lgl_(data, is_rvar)

  out = as_tibble(map_dfr_(vctrs::vec_chop(data), function(d) {
    constants = d[!rvar_cols]
    rvars = as.list(d[rvar_cols])
    cbind(constants, as.data.frame(as_draws_df(as_draws_rvars(rvars)), optional = TRUE))
  }))

  group_by_at(out, groups_)
}

#' @importFrom dplyr group_modify
nest_rvars = function(data) {
  group_modify(data, function(d, keys) {
    if (all(is.na(d[[".chain"]]))) {
      d[[".chain"]] = 1
    }
    if (all(is.na(d[[".iteration"]]))) {
      d[[".iteration"]] = d[[".draw"]]
    }
    as_tibble(as_draws_rvars(as_draws_df(d)))
  })
}
