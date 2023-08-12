# sample_draws
#
# Author: mjskay
###############################################################################


# sample_draws ---------------------------------------------------

#' Sample draws from a tidy-format data frame of draws
#'
#' Given a tidy-format data frame of draws with a column indexing each draw, subsample the data frame to a given size
#' based on a column indexing draws, ensuring that rows in sub-groups of a grouped data frame are sampled from the
#' same draws.
#'
#' `sample_draws()` makes it easier to sub-sample a grouped, tidy-format data frame of draws. On a grouped data frame,
#' the naive approach of using `filter` with the `.draw` column will give incorrect results as it will
#' select a different sample within each group. `sample_draws()` ensures the same sample is selected within
#' each group.
#'
#' @param data Data frame to sample from
#' @template param-ndraws
#' @template param-seed
#' @param draw The name of the column indexing the draws; default `".draw"`.
#' @author Matthew Kay
#' @keywords manip
#' @examples
#' \dontrun{
#'
#' library(ggplot2)
#' library(dplyr)
#' library(brms)
#' library(modelr)
#'
#' theme_set(theme_light())
#'
#' m_mpg = brm(mpg ~ hp * cyl, data = mtcars,
#'   # 1 chain / few iterations just so example runs quickly
#'   # do not use in practice
#'   chains = 1, iter = 500)
#'
#' # draw 100 fit lines from the posterior and overplot them
#' mtcars %>%
#'   group_by(cyl) %>%
#'   data_grid(hp = seq_range(hp, n = 101)) %>%
#'   add_epred_draws(m_mpg) %>%
#'   # NOTE: only use sample_draws here when making spaghetti plots; for
#'   # plotting intervals it is always best to use all draws
#'   sample_draws(100) %>%
#'   ggplot(aes(x = hp, y = mpg, color = ordered(cyl))) +
#'   geom_line(aes(y = .epred, group = paste(cyl, .draw)), alpha = 0.25) +
#'   geom_point(data = mtcars)
#'
#' }
#' @importFrom dplyr filter
#' @export
sample_draws = function(data, ndraws, draw = ".draw", seed = NULL) {
  .draw = as.name(draw)

  draw_full = data[[draw]]

  if (!is.null(seed)) set.seed(seed)

  draw_sample = sample(unique(draw_full), ndraws)

  filter(data, !!.draw %in% !!draw_sample)
}
