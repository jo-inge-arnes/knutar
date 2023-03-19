#' Print knot placements
#'
#' @param knot_placements A list with two named lists, 'knots' and
#' 'Boundary.knots'
#' @export
#' @examples
#' print_knots(knot_placements)
print_knots <- function(knot_placements) {
  R.utils::printf("Inner knots count: %d\n", length(knot_placements$knots))
  knots_str <- paste0("[", paste0(knot_placements$knots, collapse = ", "), "]")
  boundary_str <-
    paste0("[", paste0(knot_placements$Boundary.knots, collapse = ", "), "]")
  R.utils::printf("Inner knots: %s\nBoundary knots: %s\n", knots_str,
    boundary_str)
}
