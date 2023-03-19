#' Chooses the best from a set of restricted cubic spline (RCS) models with
#' an inner knot count lower than or equal to a specified maximum number
#'
#' The maximum number of inner knots is given as an input argument.
#' @param dataset The data frame
#' @param dependent The dependent variable in the formula
#' @param independents The independent variables in the formula
#' @param max_nknots The maximum number of inner knots wanted
#' @param icr_fn The information criterion function comparing models with
#' different knot counts (BIC default)
#' @param cost_fn The criterion used to choose which inner knot to remove, used
#' by the function 'choose_removal'. Default is BIC.
#' @param initial_nknots The initial high number inner of knots for the
#' algorithm (default is the value from the 'suggest_knotcount'-function)
#' @param diff_better How much lower must the score be to make a higher knot
#' model be deemed a better model than an alternative lower knot model?
#' @param all_models If TRUE, the function will include all intermediate models
#' in the results as 'all_models'. Default is FALSE.
#' @param boundary_knots The boundary knot placements or NA if not specified
#' @return The chosen 'model', 'score', 'knots', and 'all_models'
#' @export
#' @examples
#' my_model <- choose_splines(d, y, x, 7)
#' my_model <- choose_splines(d, y, x, 7, BIC)
choose_splines <- function(dataset,
                        dependent,
                        independents,
                        max_nknots = 10,
                        ...,
                        icr_fn = stats::BIC,
                        cost_fn = stats::BIC,
                        initial_nknots = -1,
                        diff_better = 0,
                        all_models = FALSE,
                        boundary_knots = NA) {
  independents <- rlang::enquo(independents)
  dependent <- rlang::enquo(dependent)


  if (missing(max_nknots)) max_nknots <- 10
  if (missing(icr_fn)) icr_fn <- stats::BIC
  if (missing(cost_fn)) cost_fn <- stats::BIC
  if (missing(initial_nknots)) initial_nknots <- -1
  if (missing(diff_better)) diff_better <- 0
  if (missing(all_models)) all_models <- FALSE
  if (missing(boundary_knots)) boundary_knots <- NA

  if (initial_nknots == -1) {
    initial_nknots <-
      suggest_knotcount(dataset, !!dependent, !!independents,
        boundary_knots = boundary_knots)$nknots
  }

  upper_model <- suggest_splines(dataset, !!dependent, !!independents,
    max_nknots,
    initial_knots = initial_nknots,
    cost_fn = cost_fn,
    boundary_knots = boundary_knots)

  cur_model <- upper_model
  best_model <- cur_model
  cur_score <- icr_fn(best_model)
  best_score <- cur_score
  best_knots <- extract_knots(best_model)
  cur_nknots <- length(best_knots$knots)
  intermediate_models <- list()

  while (cur_nknots > 0) {
    these_knots <- extract_knots(cur_model)
    chosen <- choose_removal(dataset, !!dependent, !!independents,
      these_knots$knots, these_knots$Boundary.knots, cost_fn)

    cur_score <- icr_fn(chosen$model)
    if (cur_score <= (best_score + diff_better)) {
      best_model <- chosen$model
      best_score <- cur_score
      best_knots <- extract_knots(best_model)
    }
    cur_model <- chosen$model
    cur_nknots <- length(extract_knots(cur_model)$knots)
    if (all_models) {
      intermediate_models <- append(intermediate_models, list(cur_model))
    }
  }

  return(
    list(model = best_model, score = best_score, knots = best_knots,
      all_models = intermediate_models))
}
