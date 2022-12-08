#' Check if the subinterval <a, b] for a variable is empty
#' @param d The dataset
#' @param v The variable
#' @param a The lower, exclusive limit
#' @param b The upper, inclusive limit
#' return TRUE if empty, FALSE if non-empy
is_empty_interval <- function(d, v, a, b) {
  return(nrow(subset(d, {{ v }} > a & {{ v }} <= b)) == 0)
}

#' Find best model with less than 'target_nknots' given the dataset, knots,
#' boundary knots, information criterion, and a cost function criterion.
#'
#' @param d The dataset
#' @param dependent The dependent variable in the forumla
#' @param independents The independent variables in the formula
#' @param knots The knot placements
#' @param b_knots The lower and upper boundary knots
#' @param target_nknots The maximum number of knots wanted in the final model
#' @param icr_fn The information criterion for choosing best model
#' @param cost_fn A cost function used to evaluate which knot to remove.
#' @return The best scoring model with maximum 'target_nknots' knots
remove_knots <- function(d,
                          dependent,
                          independents,
                          knots,
                          b_knots,
                          target_nknots = 7,
                          icr_fn = stats::BIC,
                          cost_fn = stats::AIC) {
  if (missing(icr_fn)) icr_fn <- stats::BIC
  if (missing(cost_fn)) icr_fn <- stats::AIC
  if (missing(target_nknots)) target_nknots <- 7

  dependent <- rlang::enquo(dependent)
  independents <- rlang::enquo(independents)


  mod <- model_by_knots(d, !!dependent, !!independents, knots, b_knots)

  nknots <- length(knots)
  chosen_model <- mod
  final_model <- mod
  final_score <- icr_fn(mod)

  suppressWarnings({
    while (nknots > 0) {
      del_candidate <- choose_removal(d, !!dependent, !!independents,
        knots, b_knots)
      knots <- knots[-del_candidate$index]
      nknots <- length(knots)

      chosen_model <- del_candidate$model
      cur_score <- icr_fn(del_candidate$model)

      if (nknots <= target_nknots && cur_score <= final_score) {
        final_model <- chosen_model
        final_score <- cur_score
      }
    }
  })

  return(final_model)
}

choose_splines_thorough <- function(d,
                                    dependent,
                                    independents,
                                    target_nknots = 7,
                                    icr_fn = stats::BIC,
                                    cost_fn = stats::AIC) {
  if (missing(icr_fn)) icr_fn <- stats::BIC
  if (missing(cost_fn)) icr_fn <- stats::AIC
  if (missing(target_nknots)) target_nknots <- 7
  dependent <- rlang::enquo(dependent)
  independents <- rlang::enquo(independents)

  res <- list()

modx <- cladina::model_by_count(d, !!dependent, !!independents, 7)
print(plot_model(d, !!dependent, !!independents, modx))

  b_knots <- c(min(d$Independent), max(d$Independent))
  unique_x <- sort(unique(d$Independent))
  n_unique <- length(unique_x)
  w <- b_knots[[2]] - b_knots[[1]]

  # One knot per unique value
  knots <- unique_x[2:(n_unique - 1)]
  mod <- remove_knots(d, !!dependent, !!independents, knots, b_knots,
    target_nknots, icr_fn, cost_fn)
  res <- append(res, list(mod))

  return(res)
}

main <- function() {
  library(ggplot2)
  library("cladina")

  file_name <- "../paper-3-package/regressionspaper/synthetic_linear.csv"
  file_name_test <-
    "../paper-3-package/regressionspaper/synthetic_linear_test.csv"

  d <- read.table(file_name, sep = ",", header = TRUE)

  choose_splines_thorough(d, Dependent, SignalRaw + Noise)

  print("ferdig")
}