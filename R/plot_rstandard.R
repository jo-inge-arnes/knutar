#' Plots the standardized residuals for the model
#'
#' @param dataset The data frame
#' @param mod The model
#' @export
#' @examples
#' plot_rstandard(d, my_mod)
plot_rstandard <- function(dataset, model) {
    ggplot2::ggplot(dataset, aes(x = stats::predict(model, dataset),
      y = stats::rstandard(model))) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth(method = "loess", formula = y ~ x)
}