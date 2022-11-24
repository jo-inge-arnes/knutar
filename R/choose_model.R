choose_model <- function(dataset,
                        dependent,
                        independents,
                        max_nknots = 10,
                        min_nknots = 1,
                        icr_fn = BIC,
                        cost_fn = AIC,
                        initial_nknots = -1,
                        diff_better = 0) {
  independents <- enquo(independents)
  dependent <- enquo(dependent)

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
  best_nknots <- extract_knots(best_model)

  for (cur_nknots in min_nknots:(max_nknots - 1)) {
    these_knots <- extract_knots(cur_model)
    chosen <- choose_removal(dataset, !!dependent, !!independents,
      these_knots$knots, these_knots$Boundary.knots, cost_fn)

    cur_score <- icr_fn(chosen$model)
    if (cur_score <= (best_score + diff_better)) {
      best_model <- chosen$model
      best_score <- cur_score
      best_nknots <- extract_knots(best_model)
    }
    cur_model <- chosen$model
  }

  return(list(model = best_model$model, score = best_score))
}