#' Chooses the best from a set of natural splines regression models with
#' a knot count lower than or equal to a specified maximum number of knots
#'
#' The maximum number of knots is given as an input argument.
#' @param dataset The data frame
#' @param dependent The dependent variable in the formula
#' @param independents The independent variables in the formula
#' @param max_nknots The maximum number of knots wanted
#' @param icr_fn The information criterion function comparing models with
#' different knot counts (AIC default)
#' @param cost_fn For comparing different models with equal knot counts
#' (default AIC)
#' @param initial_nknots The initial high number of knots for the algorithm
#' (default is the value from the 'suggest_knotcount'-function)
#' @param diff_better How much lower must the score be to make a higher knot
#' model be deemed a better model than an alternative lower knot model?
#' @return The chosen 'model', 'score', and 'nknots'
#' @export
#' @examples
#' my_model <- choose_splines(d, y, x, 7)
#' my_model <- choose_splines(d, y, x, 7, BIC)
choose_splines <- function(dataset,
                        dependent,
                        independents,
                        max_nknots = 10,
                        icr_fn = stats::AIC,
                        cost_fn = stats::AIC,
                        initial_nknots = -1,
                        diff_better = 0) {
  independents <- rlang::enquo(independents)
  dependent <- rlang::enquo(dependent)

  if (initial_nknots == -1) {
    initial_nknots <-
      suggest_knotcount(dataset, !!dependent, !!independents)$nknots
  }

  upper_model <- suggest_splines(dataset, !!dependent, !!independents,
    max_nknots, initial_nknots, cost_fn)

  cur_model <- upper_model
  best_model <- cur_model
  cur_score <- icr_fn(best_model)
  best_score <- cur_score
  best_knots <- extract_knots(best_model)

  if (length(best_knots) < max_nknots) {
    max_nknots <- length(best_knots$knots)
  }

  for (cur_nknots in 1:(max_nknots - 1)) {
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
    list(model = best_model, score = best_score, knots = best_knots))
}

#region code for debugging
# main <- function() {
#   library(tidyverse)
#   library(tidyr)
#   library("cladina")

#   d <- read.table(
#     "~/datasets/human_penguin/explorepenguin_share_complete_cases.csv",
#     sep = ",", header = TRUE)
#   d <- d %>%
#       drop_na(nwsize) %>%
#       drop_na(age) %>%
#       mutate(age_years = 2022 - age, age_dec = age_years / 10)


#   # Just to make is the same as the fields in the synthetic data
#   d$Independent <- d$age_dec
#   d$Dependent <- d$nwsize
#   d$SignalMeasured <- d$Dependent

#   # Shuffle the rows
#   set.seed(7)
#   d <- d[sample(1:nrow(d)), ]

#   # Bootstrap the data to create a training and a test set
#   n_split <- trunc(nrow(d) * 0.5)
#   d_full <- d
#   d <- d_full[1:n_split, ]
#   d_test <- d_full[(n_split + 1):nrow(d_full), ]

#   best_global_nknots <- suggest_knotcount(d, nwsize, age_dec)$nknots

#   cladina_res <- choose_splines(d, nwsize, age_dec, 10,
#     initial_nknots = best_global_nknots, diff_better = 2)
# }
#endregion