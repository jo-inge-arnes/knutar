#' Function assessing different types of regression models, returing the one
#' giving the best results according to an information criterion.
#'
#' @param dataset The data frame
#' @param dependent The dependent variable in the formula
#' @param independents The independent variables in the formula
#' @return A list with named elements 'model', 'type', 'score'
#' @export
#' @examples
#' my_model <- choose_model(d, y, x)$model
choose_model <- function(dataset, dependent, independents) {
    return(NULL)
}
