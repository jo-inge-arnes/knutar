#' Chooses the best of found models within a range of knot counts.
#'
#' The maximum number of knots for the model is given as a parameter. The
#' algorithm starts with a regression model with a high number of knots and
#' systematically removes knots until the target number of knots is reached.
#' The initial number of knots can be given as a parameter, and defaults to the
#' suggested number of knots obtained from the function 'suggest_knotcount'.
#' @param dataset The data frame
#' @param dependent The dependent variable in the formula
#' @param independents The independent variables in the formula
#' @param max_nknots The maximum wanted number of knots
#' @param icr_fn The information criterion function (BIC default)
#' @param cost_fn For comparing models with equal knot counts (default AIC)
#' @param initial_nknots The initial high number of knots for the algorithm
#' (default is the value from the 'suggest_knotcount'-function)
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
                        icr_fn = stats::BIC,
                        cost_fn = stats::AIC,
                        initial_nknots = -1,
                        diff_better = 0) {
  independents <- rlang::enquo(independents)
  dependent <- rlang::enquo(dependent)

  if (initial_nknots == -1) {
    initial_nknots <-
      suggest_knotcount(dataset, !!dependent, !!independents)$nknots
    # initial_nknots <- nrow(dataset) %/% 2
  }

  upper_model <- suggest_model(dataset, !!dependent, !!independents,
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

#   best_global_nknots <- suggest_knotcount(d, nwsize, age_dec)$num_knots

#   cladina_res <- choose_model(d, nwsize, age_dec, 10, initial_nknots =  best_global_nknots, diff_better = 2)
# }
