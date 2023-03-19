#' Utility function for restricted cubic splines that wraps the 'glm' and 'ns'
#' functions
#'
#' Creates a restricted cubic splines regression model given the wanted number
#' of inner knots
#' @param dataset The data frame
#' @param dependent The dependent variable in the formula
#' @param independents The independent variables in the formula
#' @param nknots The requested number of inner knots, excluding the boundary
#' knots
#' @param boundary_knots The boundary knot placements
#' @return The regression model
#' @importFrom splines ns
#' @export
#' @examples
#' my_model <- create_model(my_data, y, x, 7)
model_by_count <- function(dataset, dependent, independents, nknots,
                            boundary_knots = NA) {
  independents <- rlang::enquo(independents)
  dependent <- rlang::enquo(dependent)

  independents_str <- sub("~", "", deparse(independents))

  if (missing(boundary_knots)) boundary_knots <- NA

  if (length(boundary_knots) != 2) {
    model_formula <- stats::as.formula(
      paste0(rlang::as_name(dependent), " ~ ns(", independents_str,
      ", df = ", nknots + 1, ")"))
  } else {
    boundary_knots_str <- paste0(
      "c(", paste0(boundary_knots, collapse = ", "), ")")
    formula_str <- paste0(
      rlang::as_name(dependent), " ~ ns(", independents_str,
      ", df = ", nknots + 1, ", Boundary.knots = ", boundary_knots_str, ")")
    model_formula <- stats::as.formula(formula_str)
  }

  ns_model <- stats::glm(model_formula, data = dataset)

  return(ns_model)
}

#' Utility function for restricted cubic splines that wraps the 'glm' and 'ns'
#' functions
#'
#' Creates a restricted cubic spline regression model from the given placements
#' of the inner knots and boundary knots
#' @param dataset The data frame
#' @param dependent The dependent variable in the formula
#' @param independents The independent variables in the formula
#' @param knots The inner knot placements
#' @param boundary_knots The boundary knot placements
#' @return The regression model
#' @importFrom splines ns
#' @export
#' @examples
#' my_model <- model_by_knots(my_data, y, x, c(0.1, 0.2), c(0.0, 0.3))
model_by_knots <- function(dataset,
                              dependent,
                              independents,
                              knots,
                              boundary_knots) {
  independents <- rlang::enquo(independents)
  dependent <- rlang::enquo(dependent)

  independents_str <- sub("~", "", deparse(independents))

  knots_str <- paste0(
    "c(", paste0(knots, collapse = ", "), ")")
  boundary_knots_str <- paste0(
    "c(", paste0(boundary_knots, collapse = ", "), ")")
  formula_str <- paste0(
    rlang::as_name(dependent), " ~ ns(", independents_str,
    ", knots = ", knots_str, ", Boundary.knots = ", boundary_knots_str, ")")
  model_formula <- stats::as.formula(formula_str)

  ns_model <- stats::glm(model_formula, data = dataset)

  return(ns_model)
}
