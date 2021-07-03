# gather_rvars
#
# Author: mjskay
###############################################################################


# gather_rvars ------------------------------------------------------------

#' @rdname spread_rvars
#' @importFrom rlang enquos
#' @importFrom dplyr inner_join
#' @export
gather_rvars = function(model, ..., n = NULL, seed = NULL) {
  draws = sample_draws_from_rvars_(model, n, seed)

  list_of_list_of_rvar_tibbles = lapply(enquos(...), function(variable_spec) {
    spec = parse_variable_spec(variable_spec)
    gather_rvars_(draws, spec)
  })
  list_of_rvar_tibbles = unlist(list_of_list_of_rvar_tibbles, recursive = FALSE)

  out = bind_rows(list_of_rvar_tibbles)

  out
}

gather_rvars_ = function(draws, spec) {
  variable_names = spec[[1]]

  # gather_rvars uses a long format so we'll pull out each variable individually
  lapply(variable_names, function(variable_name) {
    spec[[1]] = variable_name
    rvar_tibble = spread_rvars_(draws, spec)

    # rename variable to ".value" and add a ".variable" column
    rvar_tibble[[".variable"]] = variable_name
    rvar_tibble[[".value"]] = rvar_tibble[[variable_name]]
    rvar_tibble[[variable_name]] = NULL

    # .stub col is not needed for gather_rvars (it's used for joins)
    rvar_tibble[[".stub"]] = NULL

    rvar_tibble
  })
}
