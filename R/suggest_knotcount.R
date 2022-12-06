#' Finds the number of natural spline knots that gives the lowest score for an
#' information criterion and a data set.
#'
#' @param dataset The data frame
#' @param dependent The dependent variable in the formula
#' @param independents The independent variable(s) in the formula
#' @param maximum_knots The highest knot count to assess. Defaults to n / 2
#' @param icr_fn The information criterion function. Defaults to AIC
#' @param all_scores If TRUE, all scores are returned in a list 'all_scores'
#' @return A list with named elements 'nknots', 'score', and 'all_scores'
#' @importFrom splines ns
#' @export
#' @examples
#' suggest_knotcount(d, nwsize, age_dec)
suggest_knotcount <- function(dataset,
                dependent,
                independents,
                max_nknots = -1,
                icr_fn = stats::AIC,
                all_scores = FALSE) {
  dependent <- rlang::enquo(dependent)
  independents <- rlang::enquo(independents)

  if (missing(max_nknots) || max_nknots == -1) {
    max_nknots <- nrow(dataset) %/% 2
  }
  if (missing(icr_fn)) icr_fn <- stats::AIC
  if (missing(all_scores)) all_scores <- FALSE

  min_icr <- Inf
  min_ndf <- Inf

  n_knots <- list()
  scores <- list()

  for (i in 1:(max_nknots + 1)) {
    model_formula <- stats::formula(paste0(
        rlang::as_name(dependent),
        " ~ ns(",
        rlang::as_name(independents),
        ", df = ",
        i,
        ")"))

    mod_spline <- stats::glm(model_formula, data = dataset)

    icr_score <- icr_fn(mod_spline)

    if (all_scores) {
      scores <- append(scores, icr_score)
      n_knots <- append(n_knots, i - 1)
    }

    if (icr_score < min_icr) {
      min_icr <- icr_score
      min_ndf <- i
    }
  }

  return(list(nknots = min_ndf - 1, score = min_icr,
    all_scores = list(scores = scores, n_knots = n_knots)))
}