# compare_levels
#
# Author: salwahammouch
###############################################################################

#' @importFrom tidyselect vars_pull all_of one_of
#' @importFrom rlang enquo quo_get_expr as_name eval_tidy quo_name sym .data is_expression
#' @importFrom dplyr group_by_at group_vars %>% do select bind_rows bind_cols
#' @importFrom tidyr pivot_wider
#' @importFrom tibble tibble
#' @importFrom stats setNames
#' @importFrom utils combn

# comparison types --------------------------------------------------------

comparison_types = list(
  ordered = function(x) {
    l = levels(x)
    lapply(2:length(l), function(i) c(l[[i]], l[[i - 1]]))
  },
  control = function(x) {
    l = levels(x)
    lapply(l[-1], function(j) c(j, l[[1]]))
  },
  pairwise = function(x) {
    lapply(combn(levels(x), 2, simplify = FALSE), rev)
  },
  revpairwise = function(x) {
    combn(levels(x), 2, simplify = FALSE)
  },
  default = function(x) {
    if (is.ordered(x)) comparison_types$ordered(x) else comparison_types$pairwise(x)
  }
)

# compare_levels ----------------------------------------------------------

#' @export
compare_levels = function(data, variable, by, fun = `-`, comparison = comparison_types$default,
                          draw_indices = c(".chain", ".iteration", ".draw"),
                          ignore_groups = ".row") {
  variable = tidyselect::vars_pull(names(data), !!enquo(variable))
  by = tidyselect::vars_pull(names(data), !!enquo(by))
  fun = enquo(fun)
  
  # Handle comparison argument
  if (is.character(comparison)) {
    comparison = comparison_types[[comparison]]
  } else if (is.function(comparison)) {
    comparison = comparison
  } else {
    comparison = eval_tidy(enquo(comparison))
  }
  
  groups_ = setdiff(dplyr::group_vars(data), ignore_groups)
  
  data %>%
    dplyr::group_by_at(setdiff(groups_, by)) %>%
    dplyr::do(compare_levels_(., variable, by, fun, comparison, draw_indices)) %>%
    dplyr::group_by_at(union(groups_, by))
}

# Internal function -------------------------------------------------------
compare_levels_ = function(data, variable, by, fun, comparison, draw_indices) {
  # Convertir en facteur et garder seulement les colonnes nécessaires
  data[[by]] = factor(data[[by]])
  draw_indices = intersect(draw_indices, names(data))
  data = data[, union(draw_indices, c(variable, by))]
  
  # Créer une version large des données
  data_wide = tidyr::pivot_wider(data, 
                                 names_from = tidyselect::all_of(by), 
                                 values_from = tidyselect::all_of(variable),
                                 values_fn = list)  # Garder les valeurs comme listes
  
  # Fonction pour extraire et calculer les valeurs
  calculate_diff = function(col_name1, col_name2) {
    v1 = unlist(data_wide[[col_name1]])
    v2 = unlist(data_wide[[col_name2]])
    fun(v1, v2)
  }
  
  # Déterminer le nom de la fonction pour l'affichage
  fun_name = if (is.name(rlang::quo_get_expr(fun))) {
    rlang::quo_name(fun)
  } else {
    ":"
  }
  fun = rlang::eval_tidy(fun)
  
  # Préparer les données sans les colonnes de niveaux
  by_levels = levels(data[[by]])
  data_wide_no_levels = dplyr::select(data_wide, -tidyselect::one_of(by_levels))
  
  # Obtenir les paires de niveaux à comparer
  comparison_levels = if (is.function(comparison)) {
    comparison(data[[by]])
  } else {
    comparison
  }
  
  # Fonction pour créer les comparaisons
  imap_dfr = function(.x, .f, ...) {
    result = Map(.f, .x, names(.x) %||% seq_along(.x), ...)
    dplyr::bind_rows(result)
  }
  
  # Créer les comparaisons
  imap_dfr(comparison_levels, function(levels., by_name) {
    if (rlang::is_expression(levels.) || rlang::is_call(levels.)) {
      if (is.numeric(by_name) || by_name == "") by_name = rlang::quo_name(levels.)
      tibble::tibble(
        !!by := by_name,
        !!variable := rlang::eval_tidy(levels., data_wide)
      )
    } else {
      if (is.numeric(by_name) || by_name == "") {
        by_name = paste(levels.[[1]], fun_name, levels.[[2]])
      }
      # Calculer les différences
      diff_values = calculate_diff(levels.[[1]], levels.[[2]])
      tibble::tibble(
        !!by := by_name,
        !!variable := diff_values
      )
    }
  }) %>% 
    dplyr::bind_cols(data_wide_no_levels, .)
}






