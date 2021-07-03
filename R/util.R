# Utility functions for tidybayes
#
# Author: mjskay
###############################################################################


# deparse that is guaranteed to return a single string (instead of
# a list of strings if the expression goes to multiple lines)
deparse0 = function(expr, width.cutoff = 500, ...) {
  paste0(deparse(expr, width.cutoff = width.cutoff, ...), collapse = "")
}

# Based on https://stackoverflow.com/a/14838753
# Escapes a string for inclusion in a regex
escape_regex = function(string) {
  gsub("(\\W)", "\\\\\\1", string)
}

combine_chains_for_deprecated_ = function(x) {
  x$.chain = NA_integer_
  x$.iteration = x$.draw
  x$.draw = NULL
  x
}

draw_from_chain_and_iteration_ = function(chain, iteration) {
  max_iteration = max(iteration)
  as.integer(ifelse(is.na(chain), 0, chain - 1) * max_iteration + iteration)
}

# get all variable names from an expression
# based on http://adv-r.had.co.nz/dsl.html
all_names = function(x) {
  if (is.atomic(x)) {
    NULL
  } else if (is.name(x)) {
    name = as.character(x)
    if (name == "") {
      NULL
    }
    else {
      name
    }
  } else if (is.call(x) || is.pairlist(x)) {
    children = lapply(x[-1], all_names)
    unique(unlist(children))
  } else {
    stop("Don't know how to handle type `", typeof(x), "`",
      call. = FALSE)
  }
}

# return true if there is a method (S3 or S4) for the
# given function and class signature
#' @importFrom rlang `%||%`
has_method = function(f, signature) {
  # check for S3 methods
  for (class in signature) {
    if (!is.null(utils::getS3method(f, class, optional = TRUE))) {
      return(TRUE)
    }
  }
  # No S3 => check for S4 methods
  !is.null(methods::selectMethod(f, signature, optional = TRUE))
}


# deprecations and warnings -----------------------------------------------

#' @importFrom rlang enexprs
.Deprecated_arguments = function(old_names, ..., message = "", which = -1, fun = as.character(sys.call(which))[[1]]) {
  deprecated_args = intersect(old_names, names(enexprs(...)))

  if (length(deprecated_args) > 0) {
    stop(
      "\nIn ", fun, "(): The `", deprecated_args[[1]], "` argument is deprecated.\n",
      message,

      call. = FALSE
    )
  }
}

.Deprecated_argument_alias = function(new_arg, old_arg, which = -1, fun = as.character(sys.call(which))[[1]]) {
  if (missing(old_arg)) {
    new_arg
  } else {
    new_name = quo_name(enquo(new_arg))
    old_name = quo_name(enquo(old_arg))

    warning(
      "\nIn ", fun, "(): The `", old_name, "` argument is a deprecated alias for `",
      new_name, "`.\n",
      "Use the `", new_name, "` argument instead.\n",
      "See help(\"tidybayes-deprecated\").\n",

      call. = FALSE
    )

    old_arg
  }
}

stop_on_non_generic_arg_ = function(parent_dot_args, method_type, ..., which = -1, fun = as.character(sys.call(which))[[1]]) {
  old_args = list(...)

  if (any(parent_dot_args %in% old_args)) {
    non_generic_args_passed = intersect(parent_dot_args, old_args)
    non_generic_arg_passed = non_generic_args_passed[[1]]

    stop(
      "In ", fun, "(): ",
      "The argument `", non_generic_arg_passed,
      "` is not supported in `",
      method_type,
      "`. Please use the generic argument `",
      names(old_args)[old_args == non_generic_arg_passed],
      "`. See the documentation for additional details.\n",

      call. = FALSE
    )
  }
}

# workarounds -------------------------------------------------------------

# workarounds / replacements for common patterns

map_dfr_ = function(data, fun, ...) {
  bind_rows(lapply(data, fun, ...))
}

imap_dfr_ = function(.x, .f, ...) {
  .names = names(.x) %||% seq_along(.x)
  bind_rows(mapply(.f, .x, .names, MoreArgs = list(...), SIMPLIFY = FALSE))
}

map_lgl_ = function(X, FUN, ...) {
  vapply(X, FUN, FUN.VALUE = logical(1), ...)
}

map2_ = function(X, Y, FUN) {
  mapply(FUN, X, Y, USE.NAMES = FALSE, SIMPLIFY = FALSE)
}

reduce_ = function(.x, .f, .init, ...) {
  Reduce(function(x, y) .f(x, y, ...), .x, init = .init)
}

discard_ = function(.x, .f, ...) {
  i = map_lgl_(.x, .f, ...)
  .x[is.na(i) | !i]
}
