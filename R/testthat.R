# Helpers for testthat tests
#
# Author: mjskay
###############################################################################


# skips -------------------------------------------------------------------

#' skip tests sensitive to minor changes in density() in R 4.4
#' @noRd
skip_if_sensitive_to_density = function() {
  testthat::skip_if(getRversion() < "4.4", "density() output changed in R 4.4")
}

# skip tests if there is no vdiffr setup or if the setup is likely
# to produce false positive test failures (e.g. old version of ggplot2)
skip_if_no_vdiffr = function() {
  testthat::skip_if_not_installed("vdiffr")
  testthat::skip_if_not_installed("svglite")
  testthat::skip_if_not_installed("ggplot2", "3.3.3.9000")
  # unlike ggdist tests, these tests are all for deprecated stuff so minor
  # changes in images on CI are more trouble than they are worth
  testthat::skip_on_ci()
}
