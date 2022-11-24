#' Utility function for natural splines that wraps the 'lm' and 'ns' functions
#'
#' Creates a natural splines regression model given the wanted number of knots
#' @param dataset The data frame
#' @param dependent The dependent variable in the formula
#' @param independents The independent variables in the formula
#' @param nknots The requested number of knots
#' @return The regression model
#' @export
#' @examples
#' my_model <- create_model(my_data, y, x, 7)
model_by_count <- function(dataset, dependent, independents, nknots) {
  independents <- rlang::enquo(independents)
  dependent <- rlang::enquo(dependent)
  model_formula <- stats::as.formula(
    paste0(rlang::as_name(dependent), " ~ ns(", rlang::as_name(independents),
    ", df = ", nknots + 1, ")"))

  ns_model <- stats::lm(model_formula, data = dataset)

  return(ns_model)
}

#' Utility function for natural splines that wraps the 'lm' and 'ns' functions
#'
#' Creates a natural splines regression model from the given placements of
#' the knots and boundary knots
#' @param dataset The data frame
#' @param dependent The dependent variable in the formula
#' @param independents The independent variables in the formula
#' @param knots The knot placements
#' @param knots The boundary knot placements
#' @return The regression model
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

  knots_str <- paste0(
    "c(", paste0(knots, collapse = ", "), ")")
  boundary_knots_str <- paste0(
    "c(", paste0(boundary_knots, collapse = ", "), ")")
  formula_str <- paste0(
    rlang::as_name(dependent), " ~ ns(", rlang::as_name(independents),
    ", knots = ", knots_str, ", Boundary.knots = ", boundary_knots_str, ")")
  model_formula <- stats::as.formula(formula_str)

  ns_model <- stats::lm(model_formula, data = dataset)

  return(ns_model)
}