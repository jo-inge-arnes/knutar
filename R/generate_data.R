#' Generates synthetic data
#'
#' Se the examples section for information about the returned column names
#'
#' @param n The number of units in the sample
#' @param x_accr The accuracy (number of decimals) of independent variable
#' @param y_accr The accuracy (number of decimals) of dependent variable
#' @param f_x_dist Function for the distribution of the independent variable
#' @param f_signal The true function for the population means. Takes the x
#' values as input.
#' @param f_noise The noise function, i.e., variance distribution(s) around the
#' population means for y, which can be heteroscedastic.
#' @return A dataframe with generated data
#' @export
#' @examples
#' The returned data frame has the following columns:
#' ID               # IDs
#' Independent      # The measured X values (rounded according to accuracy)
#' Dependent        # The measured Y values (rounded according to accuracy)
#' IndependentRaw   # The raw X values (not rounded)
#' DependentRaw     # The raw Y values (not rounded)
#' SignalRaw        # The raw signal part of the population mean curve for Y
#' Noise            # The raw noise (variance) for the DependentRaw values
#' SignalMeasured   # The signal rounded according to accuracy
generate_data <- function(n, x_accr, y_accr, f_x_dist,
                          f_signal, f_noise) {
  ids <- 1:n
  xs_raw <- f_x_dist(n)
  ys_signal <- f_signal(xs_raw)
  ys_noise <- f_noise(xs_raw)
  ys_raw <- ys_signal + ys_noise

  xs_measured <- xs_raw
  if (!missing(x_accr) && !is.null(x_accr) && x_accr > -1) {
    xs_measured <- round(xs_raw, x_accr)
  }

  ys_measured <- ys_raw
  if (!missing(y_accr) && !is.null(y_accr) && y_accr > -1) {
    ys_measured <- round(ys_raw, y_accr)
  }

  xs_measured_signal <- f_signal(xs_measured)

  return(data.frame(
    ID = ids,
    Independent = xs_measured,
    Dependent = ys_measured,
    IndependentRaw = xs_raw,
    DependentRaw = ys_raw,
    SignalRaw = ys_signal,
    Noise = ys_noise,
    SignalMeasured = xs_measured_signal
  ))
}
