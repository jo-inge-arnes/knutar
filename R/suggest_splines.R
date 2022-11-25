#' Suggests a natural splines regression model with knot placements that
#' can be non-uniform with respect to quantiles and widths
#'
#' The target number of knots for the model is given as an input argument. The
#' algorithm starts with a model with a high number of knots and systematically
#' removes knots until the target number of knots is reached.
#' The initial number of knots, before starting to remove knots, can be given
#' as an argument as well, but  defaults to the suggested number of knots
#' obtained from the function 'suggest_knotcount'.
#' @param dataset The data frame
#' @param dependent The dependent variable in the formula
#' @param independents The independent variables in the formula
#' @param target_nknots The target and maximum number of knots for the model
#' @param initial_nknots The number of knots initially, defaults to the
#' result from the function 'suggest_knotcount'
#' @param cost_fn The function for the selection criterion score (AIC default)
#' @return The suggested natural splines model
#' @export
#' @examples
#' my_model <- suggest_splines(d, y, x, 7)
#' my_model <- suggest_splines(d, y, x, 7, 300, BIC)
suggest_splines <- function(dataset,
                          dependent,
                          independents,
                          target_nknots,
                          initial_nknots = -1,
                          cost_fn = stats::AIC) {
  independents <- rlang::enquo(independents)
  dependent <- rlang::enquo(dependent)

  if (initial_nknots == -1) {
    initial_nknots <-
      suggest_knotcount(dataset, !!dependent, !!independents)$nknots
  }

  # Find the initial model with a high number of knots, and get the distinct
  # knot placements
  ns_model <-
    model_by_count(dataset, !!dependent, !!independents, initial_nknots)
  knots <- extract_knots(ns_model)

  # Initialize the variables that will hold the final knot placements
  final_knots <- knots$knots
  boundary_knots <- knots$Boundary.knots

  # As long as there are more knots left than the target number, remove knots
  # one by one, by always removing the knot that gives the best resulting model
  if (length(knots$knots) > target_nknots) {
    for (i in 1:(length(knots$knots) - target_nknots)) {
      rm_index <- choose_removal(dataset, !!dependent, !!independents,
        knots = final_knots, boundary_knots = boundary_knots, cost_fn)$index
      final_knots <- final_knots[-rm_index]
    }
  }

  final_mod <- model_by_knots(dataset, !!dependent, !!independents,
    knots = final_knots, boundary_knots = boundary_knots)

  return(final_mod)
}