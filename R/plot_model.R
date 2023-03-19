#' Plots the (glm) model with data, curve, confidence bands, etc.
#'
#' @param dataset The data frame
#' @param mod The model
#' @importFrom magrittr "%>%"
#' @export
#' @examples
#' plot_model(d, y, x, my_mod)
plot_model <- function(dataset, dependent, independent, model) {
  # The confidence bands has to be in the scale of the link function,
  # and we need to get it's inverse for plotting
  ilink <- stats::family(model)$linkinv

  d <- dataset
  d <- d %>%
    dplyr::mutate(pred = stats::predict(model)) %>%
    dplyr::bind_cols(stats::setNames(dplyr::as_tibble(
      stats::predict(model, newdata = d, se.fit = TRUE)[1:2]),
        c("fit_link", "se_link"))) %>%
    dplyr::mutate(fit_resp = ilink(fit_link),
      upr = ilink(fit_link + (2 * se_link)),
      lwr = ilink(fit_link - (2 * se_link)))

  fig <- ggplot2::ggplot(d,
      ggplot2::aes(x = {{ independent }}, y = {{ dependent }})) +
    ggplot2::geom_point() +
    ggplot2::geom_ribbon(data = d,
      ggplot2::aes(ymin = lwr, ymax = upr), alpha = 0.2) +
    ggplot2::geom_line(ggplot2::aes(y = pred),
      color = "blue", size = 1)

    return(fig)
}
