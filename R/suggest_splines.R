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
#' used to compare which knot should be removed, passed to choose_removal
#' @param all_knots If TRUE, then knots for all intermediate models will be
#' included in return value. Default is FALSE.
#' @param boundary_knots The boundary knot placements or NA if not specified
#' @return Returns the suggested natural splines model, or if the 'all_knots'
#' argument was TRUE, then a list with named elements 'model', 'all_knots', and
#' 'Boundary.knots' is returned.
#' @export
#' @examples
#' my_model <- suggest_splines(d, y, x, 4)
#' my_model <- suggest_splines(d, y, x, 4, 100, BIC)
suggest_splines <- function(dataset,
                          dependent,
                          independents,
                          target_nknots,
                          initial_nknots = -1,
                          cost_fn = stats::AIC,
                          all_knots = FALSE,
                          boundary_knots = NA) {
  independents <- rlang::enquo(independents)
  dependent <- rlang::enquo(dependent)
  if (missing(boundary_knots)) boundary_knots <- NA

  if (initial_nknots == -1) {
    initial_nknots <-
      suggest_knotcount(dataset, !!dependent, !!independents,
        boundary_knots)$nknots
  }

  if (missing(cost_fn)) cost_fn <- stats::AIC
  if (missing(all_knots)) all_knots <- FALSE

  # Find the initial model with a high number of knots, and get the distinct
  # knot placements
  ns_model <-
    model_by_count(dataset, !!dependent, !!independents, initial_nknots,
      boundary_knots)
  knots <- extract_knots(ns_model)

  intermediate_knots <- list()

  # Initialize the variables that will hold the final knot placements
  final_knots <- knots$knots
  boundary_knots <- knots$Boundary.knots

  if (all_knots) {
    intermediate_knots <- append(intermediate_knots, list(final_knots))
  }

  # As long as there are more knots left than the target number, remove knots
  # one by one, by always removing the knot that gives the best resulting model
  if (length(knots$knots) > target_nknots) {
    for (i in 1:(length(knots$knots) - target_nknots)) {
      rm_index <- choose_removal(dataset, !!dependent, !!independents,
        knots = final_knots, boundary_knots = boundary_knots, cost_fn)$index
      final_knots <- final_knots[-rm_index]

      if (all_knots) {
        intermediate_knots <- append(intermediate_knots, list(final_knots))
      }
    }
  }

  final_mod <- model_by_knots(dataset, !!dependent, !!independents,
    knots = final_knots, boundary_knots = boundary_knots)

  if (all_knots) {
    return(list(model = final_mod, all_knots = intermediate_knots,
      Boundary.knots = boundary_knots))
  } else {
    return(final_mod)
  }
}