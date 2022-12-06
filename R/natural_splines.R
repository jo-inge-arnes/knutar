#' Utility function for natural splines that wraps the 'lm' and 'ns' functions
#'
#' Creates a natural splines regression model given the wanted number of knots
#' @param dataset The data frame
#' @param dependent The dependent variable in the formula
#' @param independents The independent variables in the formula
#' @param nknots The requested number of knots
#' @return The regression model
#' @importFrom splines ns
#' @export
#' @examples
#' my_model <- create_model(my_data, y, x, 7)
model_by_count <- function(dataset, dependent, independents, nknots) {
  independents <- rlang::enquo(independents)
  dependent <- rlang::enquo(dependent)

  independents_str <- sub("~", "", deparse(independents))

  model_formula <- stats::as.formula(
    paste0(rlang::as_name(dependent), " ~ ns(", independents_str,
    ", df = ", nknots + 1, ")"))

  ns_model <- stats::glm(model_formula, data = dataset)

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

#region code for debugging
# main <- function() {
#   library(tidyverse)
#   library(tidyr)
#   library(splines)
#   library("cladina")

#   file_name <- "../paper-3-package/regressionspaper/synthetic_linear.csv"
#   file_name_test <-
#     "../paper-3-package/regressionspaper/synthetic_linear_test.csv"

#   d <- read.table(file_name, sep = ",", header = TRUE)

#   print("Hello")

#   mod <- model_by_count(d, DependentRaw, SignalRaw + Noise, 7)
#   mod <- model_by_knots(d, DependentRaw, SignalRaw + Noise,
#     c(0.1, 0.2), c(-1, 1))


#   mod
# }
#endregion