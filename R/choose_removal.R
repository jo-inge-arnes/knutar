#' Finds the best knot to remove from the given model
#'
#' The function searches through the knots of the given model to find the
#' inner knot yielding the best resulting score if one knot has to be removed.
#' The selection criterion function can be seen as a cost function, where
#' lower scores are better.
#' @param dataset The data frame
#' @param dependent The dependent variable in the formula
#' @param independents The independent variables in the formula
#' @param knots The knot placements
#' @param boundary_knots The boundary knot placements
#' @param cost_fn The function for the selection criterion score (BIC default)
#' @return A named list with 'index' of the chosen knot, 'model', and 'score'
#' @export
#' @examples
#' removal_index <- choose_removal(my_data, y, x, knots, boundary_knots)$index
#' removal_index <- choose_removal(my_data, y, x, knots, boundary_knots,
#'  cost_fn = function(model) { return(-summary(model)$fstatistic[[1]]) })$index
choose_removal <- function(dataset,
                                dependent,
                                independents,
                                knots,
                                boundary_knots,
                                cost_fn = stats::BIC) {
  independents <- rlang::enquo(independents)
  dependent <- rlang::enquo(dependent)

  if (missing(cost_fn)) cost_fn <- stats::BIC

  model_scores <- lapply(seq_along(knots), function(i) {
    mod <- model_by_knots(dataset, !!dependent, !!independents,
      knots = knots[-i], boundary_knots = boundary_knots)
    mod_score <- cost_fn(mod)
    return(list(model = mod, score = mod_score))
  })

  scores <- unlist(lapply(model_scores, "[[", "score"))
  index <- which.min(scores)
  min_score <- scores[[index]]

  return(list(model = model_scores[[index]][["model"]],
    score = min_score, index = index))
}
