#' Finds the number of restricted cubic spline inner knots that gives the
#' lowest score for an information criterion and a data set.
#'
#' @param dataset The data frame
#' @param dependent The dependent variable in the formula
#' @param independents The independent variable(s) in the formula
#' @param maximum_knots The highest inner knot count to assess.
#' Defaults the lower of 50 and n / 2
#' @param icr_fn The information criterion function. Defaults to BIC
#' @param all_scores If TRUE, all scores are returned in a list 'all_scores'
#' @param boundary_knots The boundary knot placements
#' @return A list with named elements 'nknots', 'score', and 'all_scores'
#' @importFrom splines ns
#' @export
#' @examples
#' suggest_knotcount(d, nwsize, age_dec)
suggest_knotcount <- function(dataset,
                              dependent,
                              independents,
                              max_nknots = -1,
                              ...,
                              icr_fn = stats::BIC,
                              all_scores = FALSE,
                              boundary_knots = NA) {
  dependent <- rlang::enquo(dependent)
  independents <- rlang::enquo(independents)

  if (missing(max_nknots) || max_nknots == -1) {
    max_nknots <- min(50, nrow(dataset) %/% 2)
  }

  if (missing(icr_fn)) icr_fn <- stats::BIC
  if (missing(all_scores)) all_scores <- FALSE
  if (missing(boundary_knots)) boundary_knots <- NA

  min_icr <- Inf
  min_ndf <- Inf

  n_knots <- list()
  scores <- list()

  independents_str <- sub("~", "", deparse(independents))

  if (length(boundary_knots) != 2) {
    boundary_knots_str <- ""
  } else {
    boundary_knots_str <- paste0(
      ", Boundary.knots = c(", paste0(boundary_knots, collapse = ", "), ")"
    )
  }

  consecutive_non_convergance <- 0

  for (i in 1:(max_nknots + 1)) {
    # Because of a change between R 4.2.0 and 4.3.0, quantiles coinciding with
    # boundary knots must now be removed.
    #
    # This caused a bit of rewriting...
    #
    # See: https://stat.ethz.ch/pipermail/r-announce/2023/000691.html
    # "bs() and ns() in the (typical) case of automatic knot construction, when
    # some of the supposedly inner knots coincide
    # with boundary knots, now moves them inside (with a warning),
    # building on PR#18442 by Ben Bolker."

    subset_data <- dataset %>%
      dplyr::filter(
        !!independents >= boundary_knots[[1]],
        !!independents <= boundary_knots[[2]]
      )

    n <- i - 1
    knots <- c()

    if (n > 0) {
      quantiles <- unique(quantile(subset_data[[rlang::as_label(independents)]],
        probs = c(0, seq(1 / n, 1, by = 1 / n))
      ))

      if (quantiles[[1]] == boundary_knots[[1]]) {
        quantiles <- quantiles[-1]
      }

      if (!length(quantiles) == 0 &&
        quantiles[[length(quantiles)]] == boundary_knots[[2]]) {
        quantiles <- quantiles[-length(quantiles)]
      }

      knots <- quantiles
    }

    model_formula_str <- NULL

    if (length(knots) > 0) {
      model_formula_str <- paste0(
        rlang::as_label(dependent),
        " ~ ns(",
        independents_str,
        ", knots = c(",
        paste(knots, collapse = ", "),
        ")",
        boundary_knots_str,
        ")"
      )
    } else {
      model_formula_str <- paste0(
        rlang::as_label(dependent),
        " ~ ns(",
        independents_str,
        ", df = 1",
        boundary_knots_str,
        ")"
      )
    }

    model_formula <- formula(model_formula_str)

    mod_spline <- NULL

    try(mod_spline <- stats::glm(model_formula, data = dataset))

    if (!is.null(mod_spline) && mod_spline$converged) {
      consecutive_non_convergance <- 0
    } else {
      consecutive_non_convergance <- consecutive_non_convergance + 1
    }

    if (consecutive_non_convergance == 0) {
      icr_score <- icr_fn(mod_spline)

      if (all_scores) {
        scores <- append(scores, icr_score)
        n_knots <- append(n_knots, i - 1)
      }

      if (icr_score < min_icr) {
        min_icr <- icr_score
        min_ndf <- i
      }
    } else if (consecutive_non_convergance >= 3) {
      warning(paste(
        "Models failed to converge three consecutive times,",
        "will not assess any higher knot counts."
      ))
      break
    }
  }

  return(list(
    nknots = min_ndf - 1, score = min_icr,
    all_scores = list(scores = scores, n_knots = n_knots)
  ))
}
