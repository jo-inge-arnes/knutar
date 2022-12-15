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
  ys_noise <- f_noise(ys_signal)
  ys_raw <- ys_signal + ys_noise

  xs_measured <- xs_raw
  if (!missing(xs_raw) && xs_raw != NULL) {
    xs_measured <- round(xs_raw, x_accr)
  }

  ys_measured <- ys_raw
  if (!missing(ys_raw) && ys_raw != NULL) {
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

#' A function that can be passed to 'generate_data' as the 'f_x_dist' parameter.
#' Normally distributed.
#' @param n The number of units in the sample
#' @return The raw values for x
#' @export
f_x_rnorm_dist <- function(n) {
  return(rnorm(n))
}

#' A function that can be passed to 'generate_data' as the 'f_x_dist' parameter.
#' Lognormal distribution with mean = 0 and standard deviation = 1
#' @param n The number of units in the sample
#' @return The raw values for x
#' @export
f_x_lognorm_dist <- function(n) {
  return(rlnorm(n))
}

#' Function for the relationship between independent and dependent variable,
#' which can be passed to 'generate_data' as the 'f_signal' parameter.
#'
#' This signal is based on a cosine curve.
#' @param xs The x values
#' @return The raw signal part of the y values
#' @export
f_signal_cos <- function(xs) {
  return(cos(1.5 * pi * xs))
}

#' Function for the relationship between independent and dependent variable,
#' which can be passed to 'generate_data' as the 'f_signal' parameter.
#'
#' This signal is linear.
#' @param xs The x values
#' @return The raw signal part of the y values
#' @export
f_signal_linear <- function(xs) {
  return(xs * 1.5)
}

#' Function for the relationship between independent and dependent variable,
#' which can be passed to 'generate_data' as the 'f_signal' parameter.
#'
#' This signal is based on the Michaelis-Menten equation with
#' K_m = 2.0 mL, V_max = 0.5 mM/min.
#' @param xs The x values
#' @return The raw signal part of the y values
#' @export
f_signal_michaelis_menten <- function(xs) {
  v_max <- 0.5
  k_m <- 2.0
  return ((xs * v_max) / (xs + k_m))
}

#' Function for the noise (variance) to add to the signal that can be passed to
#' 'generate_data' as the 'f_noise' parameter.
#'
#' This function is based on drawing a values from a normal distribution and
#' multiplying by the y values, giving a variance that scales with y.
#' @param ys The y (dependent) values for which to generate noise/randomness
#' @return The noise/randomness to add to the y values (dependent variable's
#' population means)
#' @export
f_noise_rnorm_scaled <- function(ys) {
  return(rnorm(length(ys)) * ys)
}
