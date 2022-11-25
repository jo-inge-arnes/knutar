#' Function assessing different types of regression models, returning the one
#' giving the best results according to an information criterion
#'
#' @param knot_placements A list with two named lists, 'knots' and
#' 'Boundary.knots'
#' @export
#' @examples
#' print_knots(knot_placements)
print_knots <- function(knot_placements) {
  R.utils::printf("Knots count: %d\n", length(knot_placements$knots))
  knots_str <- paste0("[", paste0(knot_placements$knots, collapse = ", "), "]")
  boundary_str <-
    paste0("[", paste0(knot_placements$Boundary.knots, collapse = ", "), "]")
  R.utils::printf("Knots: %s Boundary knots: %s\n", knots_str, boundary_str)
}
