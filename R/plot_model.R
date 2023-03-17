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

#region code for debugging
# main <- function() {
#   library(dplyr)

#   # d <- read.table(
#   #   "~/datasets/human_penguin/explorepenguin_share_complete_cases.csv",
#   #   sep = ",", header = TRUE)
#   # d <- d %>%
#   #     drop_na(nwsize) %>%
#   #     drop_na(age) %>%
#   #     mutate(age_years = 2022 - age, age_dec = age_years / 10)


#   # # Just to make is the same as the fields in the synthetic data
#   # d$Independent <- d$age_dec
#   # d$Dependent <- d$nwsize
#   # d$SignalMeasured <- d$Dependent

#   # # Shuffle the rows
#   # set.seed(7)
#   # d <- d[sample(1:nrow(d)), ]

#   # # Bootstrap the data to create a training and a test set
#   # n_split <- trunc(nrow(d) * 0.5)
#   # d_full <- d
#   # d <- d_full[1:n_split, ]
#   # d_test <- d_full[(n_split + 1):nrow(d_full), ]

#   # Synthetic data sets

#   file_name <- "../paper-3-package/regressionspaper/synthetic_linear.csv"
#   file_name_test <- "../paper-3-package/regressionspaper/synthetic_linear_test.csv"

#   d <- read.table(file_name, sep = ",", header = TRUE)
#   d_test <- read.table(file_name_test, sep = ",", header = TRUE)

#   mod <- knutar::model_by_count(d, Dependent, Independent, 4)
#   knutar::plot_model(d, Dependent, Independent, mod)

#   print("ok")
# }
#endregion