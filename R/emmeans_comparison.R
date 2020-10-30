# emmeans_comparison
#
# Author: mjskay
###############################################################################

#' Use emmeans contrast methods with compare_levels
#'
#' Convert [emmeans contrast methods][emmeans::contrast-methods] into comparison functions
#' suitable for use with [compare_levels()].
#'
#' Given an [emmeans contrast method][emmeans::contrast-methods] name as a string
#' (e.g., `"pairwise"`, `"trt.vs.ctrl"`, etc) or an emmeans-style contrast function
#' (e.g., [emmeans::pairwise.emmc], [emmeans::trt.vs.ctrl.emmc], etc), `emmeans_comparison()`
#' returns a new function that can be used in the `comparison` argument to `compare_levels()`
#' to compute those contrasts.
#'
#' @param method An emmeans-style contrast method. One of: (1) a string specifying
#'   the name of an [emmeans contrast method][emmeans::contrast-methods], like
#'   `"pairwise"`, `"trt.vs.ctrl"`, `"eff"`; or (2) an emmeans-style contrast
#'   function itself, like [emmeans::pairwise.emmc], [emmeans::trt.vs.ctrl.emmc], etc,
#'   or a custom function that takes a vector of factor levels and returns a
#'   contrast matrix.
#' @param ... Arguments passed on to the contrast method.
#'
#' @return A function that takes a single argument, `var`, containing a variable
#' to generate contrasts for (e.g., a factor or a character vector) and returns
#' a function that generates a list of named unevaluated expressions representing
#' different contrasts of that variable. This function is suitable to be used
#' as the `comparison` argument in `compare_levels()`.
#'
#' @author Matthew Kay
#' @seealso [compare_levels()], [emmeans::contrast-methods].
#' See [gather_emmeans_draws()] for a different approach to using `emmeans` with
#' `tidybayes`.
#' @examples
#'
#' if (requireNamespace("emmeans", quietly = TRUE)) {
#'
#'   library(dplyr)
#'   library(ggplot2)
#'
#'   data(RankCorr, package = "ggdist")
#'
#'   # emmeans contrast methods return matrices. E.g. the "eff" comparison
#'   # compares each level to the average of all levels:
#'   emmeans:::eff.emmc(c("a","b","c","d"))
#'
#'   # tidybayes::compare_levels() can't use a contrast matrix like this
#'   # directly; it takes arbitrary expressions of factor levels. But
#'   # we can use `emmeans_comparison` to generate the equivalent expressions:
#'   emmeans_comparison("eff")(c("a","b","c","d"))
#'
#'   # We can use the "eff" comparison type with `compare_levels()` as follows:
#'   RankCorr %>%
#'     spread_draws(b[i,j]) %>%
#'     filter(j == 1) %>%
#'     compare_levels(b, by = i, comparison = emmeans_comparison("eff")) %>%
#'     median_qi()
#' }
#'
#' @importFrom purrr reduce map2
#' @export
emmeans_comparison = function(method, ...) {
  if (is.function(method)) {
    comp_fun = method
  } else {
    if (!requireNamespace("emmeans", quietly = TRUE)) {
      stop("The `emmeans` package is needed for `emmeans_comparison` to support emmeans contrast methods.", call. = FALSE) # nocov
    }
    comp_fun = getFromNamespace(paste0(method, ".emmc"), "emmeans")
  }

  function(var) {
    comp_matrix = comp_fun(as.character(unique(var)), ...)
    var_names = rownames(comp_matrix)
    lapply(comp_matrix, function(coefs) {
      reduce(map2(coefs, var_names, function(coef, var_name) {
        call("*", coef, as.name(var_name))
      }), ~ call("+", .x, .y))
    })
  }
}
