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

  suppressWarnings({
    mod <- model_by_knots(d, !!dependent, !!independents, knots, b_knots)

    nknots <- length(knots)
    chosen_model <- mod
    final_model <- mod
    final_score <- Inf

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
#' @param d The data frame
#' @param dependent The dependent variable in the formula
#' @param independents The independent variables in the formula
#' @param max_nknots The maximum number of knots wanted
#' @param icr_fn The information criterion function comparing models with
#' different knot counts (BIC default)
#' @param cost_fn The criterion used to choose which knot to remove, used by
#' the function 'choose_removal'. Default is AIC.
#' @param verbose Print output about progress and results (Default TRUE)
#' @return The resulting 'model', 'score', etc. of the best scoring model, and
#' lists with information about the other assessed models. See examples.
#' @importFrom lazyeval f_eval
#' @export
#' @examples
#' my_model <- choose_splines_thorough(d, y, x)$model
#' result <- choose_splines_thorough(d, y, x, icr_fn = AIC, verbose = FALSE)
#'
#' ret <- choose_splines_thorough(d, y, x)
#'
#' ret$labels[[ret$type]] # Human readable name of chosen model type
#' ret[[ret$type]]        # Gives more values for the chosen model if available
#'
#' ret$model      # The chosen model
#' ret$score      # The chosen model's score
#' ret$type       # The type of model chosen as a string
#' ret$nknots     # The number of knots
#' ret$knots      # The 'knots' and 'Boundary.knots'
#' ret$score_name # The type of score used as a string
#' ret$score_fn   # The function used for scores
#'
#' ret$labels     # Description string for the types of models
#'
#' ret$quantile        # Model obtained from a start model based on quantiles
#' ret$quantile$model  # The model
#' ret$quantile$score  # The score
#' ret$quantile$nknots     # The number of knots
#' ret$quantile$knots      # Knots and boundary knots locations
#' ret$quantile$knots$knots          # Knot locations in a list
#' ret$quantile$knots$Boundary.knots  # Boundary knot locations in a list
#'
#' res$uniform
#' res$distinct
choose_splines_thorough <- function(d,
                                    dependent,
                                    independents,
                                    max_nknots = 7,
                                    icr_fn = stats::BIC,
                                    cost_fn = stats::AIC,
                                    verbose = TRUE) {
  score_type <- NULL
  if (missing(icr_fn)) {
    icr_fn <- stats::BIC
    score_type <- "BIC"
  } else {
    score_type <- deparse(substitute(icr_fn))
  }
  if (missing(cost_fn)) cost_fn <- stats::AIC
  if (missing(max_nknots)) max_nknots <- 7
  if (missing(verbose)) verbose <- TRUE

  if (verbose) {
    R.utils::printf(
      "Assessing several approaches to finding a model suggestion ")
    R.utils::printf(
      "with at most %d knots. Warning: This may take a while...\n", max_nknots)
    R.utils::printf("-----------------------------------------------------\n")
  }

  dependent <- rlang::enquo(dependent)
  independents <- rlang::enquo(independents)

  ret_desc <- list(
    "uniform" = "The start model was based on uniformly distanced knots",
    "distinct" = "The start model was based on one knot per distict value",
    "quantile" = "The start model was based on the best scoring quantile size")
  ret <-
    list(labels = ret_desc, score_fn = icr_fn, score_name = score_type)

  # Evaluate the right-hand sided formula which represents the X values
  independents_evaluated <- lazyeval::f_eval(independents, data = d)

  b_knots <- c(min(independents_evaluated), max(independents_evaluated))
  unique_x <- sort(unique(independents_evaluated))
  n_unique <- length(unique_x)
  w <- b_knots[[2]] - b_knots[[1]]

  # ----------------------------------------------------------------------------
  # One knot per distinct value
  # ----------------------------------------------------------------------------
  if (verbose) {
    R.utils::printf("Finding model from a start model with one knot per ")
    R.utils::printf("distinct value. The number of distict values is %d.\n",
      n_unique)
  }

  knots <- unique_x[2:(n_unique - 1)]

  # Find best model with 'max_nknots' knots
  unique_mod <- remove_knots(d, !!dependent, !!independents,
    knots, b_knots, max_nknots, icr_fn, cost_fn)

  unique_score <- icr_fn(unique_mod)
  unique_knots <- extract_knots(unique_mod)
  unique_knots_cnt <- length(unique_knots$knots)

  if (verbose) {
    R.utils::printf("The resulting %s score was %f, with %d knots.\n",
      score_type, unique_score, unique_knots_cnt)
    R.utils::printf("-----------------------------------------------------\n")
  }

  ret <- append(ret,
    list(distinct = list(model = unique_mod, score = unique_score,
      nknots = unique_knots_cnt, knots = unique_knots)))

  # ----------------------------------------------------------------------------
  # Uniform distances with merged empty intervals
  # ----------------------------------------------------------------------------
  dist <- w / (n_unique - 1)
  knots <-
    seq(from = b_knots[[1]], to = b_knots[[2]], by = dist)

  if (verbose) {
    R.utils::printf("Finding model from a start model with uniform distances ")
    R.utils::printf("of %f between each knot.\n", dist)
  }

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

  # Find best model with at most 'max_nknots' knots
  uniform_mod <- remove_knots(d, !!dependent, !!independents,
    knots, b_knots, max_nknots, icr_fn, cost_fn)

  uniform_score <- icr_fn(uniform_mod)
  uniform_knots <- extract_knots(uniform_mod)
  uniform_knots_cnt <- length(uniform_knots$knots)

  if (verbose) {
    R.utils::printf("The resulting %s score was %f, with %d knots.\n",
      score_type, uniform_score, uniform_knots_cnt)
    R.utils::printf("-----------------------------------------------------\n")
  }

  ret <- append(ret,
    list(uniform = list(model = uniform_mod, score = uniform_score,
      nknots = uniform_knots_cnt, knots = uniform_knots)))

  # ----------------------------------------------------------------------------
  # Start with best knot count obtained for various same-sized quantiles
  # ----------------------------------------------------------------------------
  max_nknots <- nrow(d) %/% 2
  suggested_nknots_res <- suggest_knotcount(d, !!dependent, !!independents,
    max_nknots = max_nknots, icr_fn = icr_fn)
  suggested_knot_cnt <- suggested_nknots_res$nknots

  if (verbose) {
    R.utils::printf("Finding model from a start model with the best %s ",
      score_type)
    R.utils::printf(
      "for [0, %d] knots, which had %d knots and a score of %f.\n",
      max_nknots, suggested_knot_cnt, suggested_nknots_res$score)
  }

  mod <- model_by_count(d,  !!dependent, !!independents, suggested_knot_cnt)
  extract_knots <- extract_knots(mod)
  knots <- extract_knots$knots
  b_knots <- extract_knots$Boundary.knots
  quantile_mod <-
    remove_knots(d, !!dependent, !!independents, knots, b_knots, max_nknots,
      icr_fn, cost_fn)

  quantile_score <- icr_fn(quantile_mod)
  quantile_knots <- extract_knots(quantile_mod)
  quantile_knots_cnt <- length(quantile_knots$knots)

  if (verbose) {
    R.utils::printf("The resulting %s score was %f, with %d knots.\n",
      score_type, quantile_score, quantile_knots_cnt)
    R.utils::printf("-----------------------------------------------------\n")
  }

  ret <- append(ret,
    list(quantile = list(model = quantile_mod, score = quantile_score,
      nknots = quantile_knots_cnt, knots = quantile_knots)))

  if ((quantile_score <= unique_score) && (quantile_score <= uniform_score)) {
    ret <- append(ret,
      list(model = quantile_mod, type = "quantile", score = quantile_score,
        nknots = quantile_knots_cnt, knots = quantile_knots))
  } else if (unique_score <= uniform_score) {
    ret <- append(ret,
      list(model = unique_mod, type = "distinct", score = unique_score,
        nknots = unique_knots_cnt, knots = unique_knots))
  } else {
    ret <- append(ret, list(model = uniform_mod, type = "uniform",
      score = uniform_score, nknots = uniform_knots_cnt, knots = uniform_knots))
  }

  if (verbose) {
    R.utils::printf(
      "The chosen model was of type '%s' with %s = %f and %d knots.\n",
      ret$type, ret$score_name, ret$score, unique_knots_cnt)

      print("Tip: Visualize with 'plot_models' and 'plot_rstandard' functions")
  }

  return(ret)
}

# main <- function() {
#   library(ggplot2)
#   library("cladina")

#   file_name <- "../paper-3-package/regressionspaper/synthetic_linear.csv"
#   file_name_test <-
#     "../paper-3-package/regressionspaper/synthetic_linear_test.csv"

#   d <- read.table(file_name, sep = ",", header = TRUE)

#   choose_splines_thorough(d, Dependent, Independent)

#   print("ferdig")
# }