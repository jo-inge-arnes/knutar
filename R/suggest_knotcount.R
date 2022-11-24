#' Finds the number of natural spline knots that gives the lowest score for an
#' information criterion and a data set.
#'
#' @param dataset The data frame
#' @param dependent The dependent variable in the formula
#' @param independents The independent variable(s) in the formula
#' @param maximum_knots The highest knot count to assess. Defaults to 300
#' @param info_crit The information criterion function. Defaults to AIC
#' @return A list with named elements 'num_knots' and 'score'
#' @export
#' @examples
#' suggest_knotcount(d, nwsize, age_dec)
suggest_knotcount <- function(dataset,
                dependent,
                independents,
                maximum_knots = 300,
                info_crit = stats::AIC) {
  dependent <- rlang::enquo(dependent)
  independents <- rlang::enquo(independents)

  min_icr <- Inf
  min_ndf <- Inf

  for (i in 2:(maximum_knots + 1)) {
    model_formula <-
      stats::formula(paste0(
        rlang::as_name(dependent),
        " ~ ns(",
        rlang::as_name(independents),
        ", df = ",
        i,
        ")"))

    mod_spline <- stats::lm(model_formula, data = dataset)

    icr_score <- info_crit(mod_spline)

    if (icr_score < min_icr) {
      min_icr <- icr_score
      min_ndf <- i
    }
  }

  return(list(num_knots = min_ndf - 1, score = min_icr))
}