#' Chooses the best of found models within a range of knots.
#'
#' The target number of knots for the model is given as a parameter. The
#' algorithm starts with a regression model with a high number of knots and
#' systematically removes knots until the target number of knots is reached.
#' The initial number of knots can be given as a parameter, and defaults to
#' half of the number of rows in the dataset, using integer division by two.
#' @param dataset The data frame
#' @param dependent The dependent variable in the formula
#' @param independents The independent variables in the formula
#' @param max_nknots The maximum wanted number of knots
#' @param min_knots The minimum wanted number of knots
#' @param icr_fn The information criterion function (BIC default)
#' @param cost_fn For comparing models with equal knot counts (default AIC)
#' @param initial_nknots The initial high number of knots for the algorithm
#' (default is nrow(dataset) %/% 2)
#' @param diff_better How much lower must the score be for a higher knot count
#' model to be considered a better model than a lower knot model?
#' @return The suggested chosen 'model', 'score', and 'nknots'
#' @export
#' @examples
#' my_model <- suggest_model(d, y, x, 7)
#' my_model <- suggest_model(d, y, x, 7, 300, BIC)
choose_model <- function(dataset,
                        dependent,
                        independents,
                        max_nknots = 10,
                        min_nknots = 1,
                        icr_fn = stats::BIC,
                        cost_fn = stats::AIC,
                        initial_nknots = -1,
                        diff_better = 0) {
  independents <- rlang::enquo(independents)
  dependent <- rlang::enquo(dependent)

  if (initial_nknots == -1) {
    initial_nknots <- nrow(dataset) %/% 2
  }

  min_max_vals <- c(min_nknots, max_nknots)
  max_nknots <- max(min_max_vals)
  min_nknots <- min(min_max_vals)
  if (min_nknots < 1) {
    min_nknots <- 1
  }

  upper_model <- suggest_model(dataset, !!dependent, !!independents,
    max_nknots, initial_nknots, cost_fn)

  cur_model <- upper_model
  best_model <- cur_model
  cur_score <- icr_fn(best_model)
  best_score <- cur_score
  best_knots <- extract_knots(best_model)

  for (cur_nknots in min_nknots:(max_nknots - 1)) {
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
  }

  return(
    list(model = best_model$model, score = best_score, knots = best_knots))
}