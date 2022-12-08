#' Check if the subinterval <a, b] for a variable is empty
#' @param v A list with the variable's values
#' @param a The lower, exclusive limit
#' @param b The upper, inclusive limit
#' return TRUE if empty, FALSE if non-empy
is_empty_interval <- function(v, a, b) {
  return(!any(v > a & v <= b))
}

#' Find best model with less than 'max_nknots' given the dataset, knots,
#' boundary knots, information criterion, and a cost function criterion.
#'
#' @param d The dataset
#' @param dependent The dependent variable in the forumla
#' @param independents The independent variables in the formula
#' @param knots The knot placements
#' @param b_knots The lower and upper boundary knots
#' @param max_nknots The maximum number of knots wanted in the final model
#' @param icr_fn The information criterion for choosing best model
#' @param cost_fn A cost function used to evaluate which knot to remove.
#' @return The best scoring model with maximum 'max_nknots' knots
remove_knots <- function(d,
                          dependent,
                          independents,
                          knots,
                          b_knots,
                          max_nknots = 7,
                          icr_fn = stats::BIC,
                          cost_fn = stats::AIC) {
  if (missing(icr_fn)) icr_fn <- stats::BIC
  if (missing(cost_fn)) icr_fn <- stats::AIC
  if (missing(max_nknots)) max_nknots <- 7

  dependent <- rlang::enquo(dependent)
  independents <- rlang::enquo(independents)

  mod <- model_by_knots(d, !!dependent, !!independents, knots, b_knots)

  nknots <- length(knots)
  chosen_model <- mod
  final_model <- mod
  final_score <- Inf

  suppressWarnings({
    while (nknots > 0) {
      del_candidate <- choose_removal(d, !!dependent, !!independents,
        knots, b_knots)
      knots <- knots[-del_candidate$index]
      nknots <- length(knots)

      chosen_model <- del_candidate$model
      cur_score <- icr_fn(del_candidate$model)

      if (nknots <= max_nknots && cur_score <= final_score) {
        final_model <- chosen_model
        final_score <- cur_score
      }
    }
  })

  return(final_model)
}

#' Uses several approaches to find the best of a set of natural cubic splines 
#' regression models with a knot count lower than or equal to a specified 
#' maximum number of knots. 
#'
#' The maximum number of knots is given as an input argument.
#' @param dataset The data frame
#' @param dependent The dependent variable in the formula
#' @param independents The independent variables in the formula
#' @param max_nknots The maximum number of knots wanted
#' @param icr_fn The information criterion function comparing models with
#' different knot counts (BIC default)
#' @param cost_fn The criterion used to choose which knot to remove, used by
#' the function 'choose_removal'. Default is AIC.
#' @return List of models
#' @export
#' @examples
#' 
choose_splines_thorough <- function(d,
                                    dependent,
                                    independents,
                                    max_nknots = 7,
                                    icr_fn = stats::BIC,
                                    cost_fn = stats::AIC) {
  if (missing(icr_fn)) icr_fn <- stats::BIC
  if (missing(cost_fn)) icr_fn <- stats::AIC
  if (missing(max_nknots)) max_nknots <- 7
  dependent <- rlang::enquo(dependent)
  independents <- rlang::enquo(independents)

  independents_evaluated <- lazyeval::f_eval(independents, data = d)

  b_knots <- c(min(independents_evaluated), max(independents_evaluated))
  unique_x <- sort(unique(independents_evaluated))
  n_unique <- length(unique_x)
  w <- b_knots[[2]] - b_knots[[1]]

  # ----------------------------------------------------------------------------
  # One knot per unique value
  # ----------------------------------------------------------------------------  
  knots <- unique_x[2:(n_unique - 1)]
  from_unique_vals_mod <- remove_knots(d, !!dependent, !!independents, knots, b_knots,
    max_nknots, icr_fn, cost_fn)

  # ----------------------------------------------------------------------------
  # Uniform distances with merged empty intervals
  # ----------------------------------------------------------------------------  
  knots <- 
    seq(from = b_knots[[1]], to = b_knots[[2]], by = (w / (n_unique - 1)))

  # Remove empty intervals
  i <- 2
  while (i <= length(knots)) {
    if (is_empty_interval(independents_evaluated, knots[[i - 1]], knots[[i]])) {
      knots <- knots[-i]
    } else {
      i <- i + 1
    }
  }

  # Knots without the boundary knots already present in b_knots
  knots <- knots[2:(length(knots) - 1)]
  n_knots <- length(knots)

  from_uniform_distances_mod <- remove_knots(d, !!dependent, !!independents, knots, b_knots,
    max_nknots, icr_fn, cost_fn)

  # ----------------------------------------------------------------------------
  # Start with best knot count obtained for various same-sized quantiles
  # ----------------------------------------------------------------------------  
  suggested_knot_cnt <- 
    suggest_knotcount(d, Dependent, Independent, n_unique)$nknots
  mod <- model_by_count(d, Dependent, Independent, suggested_knot_cnt)
  extracted_knots <- extract_knots(mod)
  knots <- extracted_knots$knots
  b_knots <- extracted_knots$Boundary.knots

  from_best_quantile_knots_mod <- 
    remove_knots(d, !!dependent, !!independents, knots, b_knots, max_nknots, 
      icr_fn, cost_fn)

  return(list(from_unique_vals = from_unique_vals_mod,
              from_uniform_dists = from_uniform_distances_mod,
              from_best_quantile_knots = from_best_quantile_knots_mod))
}

main <- function() {
  library(ggplot2)
  library("cladina")

  file_name <- "../paper-3-package/regressionspaper/synthetic_linear.csv"
  file_name_test <-
    "../paper-3-package/regressionspaper/synthetic_linear_test.csv"

  d <- read.table(file_name, sep = ",", header = TRUE)

  choose_splines_thorough(d, Dependent, Independent)

  print("ferdig")
}