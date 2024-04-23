# gather_draws
#
# Author: mjskay
###############################################################################


# gather_draws ------------------------------------------------------------

#' @rdname spread_draws
#' @importFrom dplyr bind_rows group_by_at
#' @importFrom rlang enquos
#' @export
gather_draws = function(
  model,
  ...,
  regex = FALSE,
  sep = "[, ]",
  ndraws = NULL,
  seed = NULL,
  draw_indices = c(".chain", ".iteration", ".draw"),
  n
) {
  ndraws = .Deprecated_argument_alias(ndraws, n)

  draws = sample_draws_from_model_(model, ndraws, seed)

  draw_indices = intersect(draw_indices, names(draws))
  tidysamples = lapply(enquos(...), function(variable_spec) {
    gather_variables(
      spread_draws_(draws, variable_spec, regex = regex, sep = sep, draw_indices = draw_indices),
      exclude = c(draw_indices, ".row")
    )
  })

  #get the groups from all the samples --- when we bind them together,
  #the grouping information is not always retained, so we'll have to recreate
  #the full set of groups from all the data frames after we bind them
  groups_ = tidysamples %>%
    lapply(group_vars) %>%
    reduce_(union)

  bind_rows(tidysamples) %>%
    group_by_at(groups_)
}
