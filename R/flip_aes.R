# flip_aes, for deprecated point_intervalh functions
#
# Originally based on https://github.com/lionel-/ggstance/blob/master/R/flip-aes.R by lionel-
# Modified to add flip_aes.function, customizable lookup table and horizontal/vertical lookups by mjskay
###############################################################################

horizontal_aes_lookup = c(
  ymin = "xmin", ymax = "xmax", yend = "xend", y = "x",
  yintercept = "xintercept",
  ymin_final = "xmin_final", ymax_final = "xmax_final",
  y_scales = "x_scales",
  SCALE_Y = "SCALE_X",
  lower = "xlower", middle = "xmiddle", upper = "xupper"
)

vertical_aes_lookup = c(
  xmin = "ymin", xmax = "ymax", xend = "yend", x = "y",
  xintercept = "yintercept",
  xmin_final = "ymin_final", xmax_final = "ymax_final",
  x_scales = "y_scales",
  SCALE_X = "SCALE_Y",
  xlower = "lower", xmiddle = "middle", xupper = "upper"
)

flip_aes_lookup = c(horizontal_aes_lookup, vertical_aes_lookup)


flip_aes = function(x, lookup = flip_aes_lookup) {
  UseMethod("flip_aes")
}

#' @export
flip_aes.character = function(x, lookup = flip_aes_lookup) {
  flipped = lookup[x]
  x[!is.na(flipped)] = flipped[!is.na(flipped)]
  x
}

#' @export
flip_aes.data.frame = function(x, lookup = flip_aes_lookup) {
  names(x) = flip_aes(names(x), lookup = lookup)
  x
}

#' @export
flip_aes.function = function(x, lookup = flip_aes_lookup) {
  name = force(deparse(substitute(x)))
  function(...) {
    .Deprecated(name, package = "tidybayes")
    flip_aes(x(...), lookup = lookup)
  }
}
