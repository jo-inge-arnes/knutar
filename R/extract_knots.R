#' Extracts the distinct knot placements of natural spline regression models
#'
#' @param ns_model The natural spline regression model
#' @return A list with named elements 'knots' and 'Boundary.knots'
#' @keywords knots, splines, regressions
#' @export
#' @examples
#' extract_knots(my_model)
extract_knots <- function(ns_model) {
  knots <- attr(ns_model$model[[2]], "knots")
  knots <- unique(knots)
  bknots <- attr(ns_model$model[[2]], "Boundary.knots")
  return(list(knots = knots, Boundary.knots = bknots))
}