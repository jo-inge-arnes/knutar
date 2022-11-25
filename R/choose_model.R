#' Function assessing different types of regression models, returning the one
#' giving the best results according to an information criterion
#'
#' @param dataset The data frame
#' @param dependent The dependent variable in the formula
#' @param independents The independent variables in the formula
#' @param icr_fn The information criterion function (default AIC)
#' @param fp_alpha The relax factor for multivariate fractional polynomials
#' @param max_nsknots The max number of knots for natural splines (default 7)
#' @param max_fp_df The max degrees of freedom for fractional polynomials
#' (default 7)
#' @param verbose Verbose output, default TRUE
#' @return A list with named elements 'model', 'type', 'score'
#' @importFrom stats glm
#' @importFrom mfp fp
#' @export
#' @examples
#' my_model <- choose_model(d, y, x)$model
#' result <- choose_model(d, y, x, icr_fn = BIC, verbose = TRUE)
choose_model <- function(dataset,
                        dependent,
                        independents,
                        icr_fn = stats::BIC,
                        fp_alpha = NA,
                        max_nsknots = 7,
                        max_fp_df = 4,
                        verbose = TRUE) {
  dependent <- rlang::enquo(dependent)
  independents <- rlang::enquo(independents)

  # Multivariate fractional polynomials (move to separate func)
  fp_formula <- stats::formula(paste0(
    rlang::as_name(dependent),
    " ~ fp(",
    rlang::as_name(independents),
    ", df = ",
    max_fp_df,
    ")"
  ))
  mfp_res <- NA
  if (is.na(fp_alpha)) {
    mfp_res <-
      mfp::mfp(fp_formula, data = dataset, verbose = verbose)
  } else {
    mfp_res <-
      mfp::mfp(fp_formula, alpha = fp_alpha, data = dataset, verbose = verbose)
  }
  mfp_mod <- eval(summary(mfp_res)$call)
  mfp_score <- icr_fn(mfp_mod)

  score_type <- deparse(substitute(icr_fn))
  ret_desc <- list(
    "mfp" = "Multivariate fractional polynomials",
    "ns_nu" = "Natural splines with freely placed knots",
    "ns" = "Natural splines with knots placed at quantiles")

  if (verbose) {
    R.utils::printf("-----------------------------------------------------\n")
    R.utils::printf("%s\n%s: %f\n\n", ret_desc[["mfp"]], score_type, mfp_score)
  }

  # Natural splines with knots distanced by equally sized bins (quantiles)
  knotcnt_suggestion <-
    suggest_knotcount(dataset, !!dependent, !!independents, max_nsknots)
  ns_mod <- model_by_count(dataset, !!dependent, !!independents,
    knotcnt_suggestion$nknots)
  ns_score <- icr_fn(ns_mod)


  if (verbose) {
    R.utils::printf("%s\n%s: %f\n", ret_desc[["ns"]], score_type, ns_score)
    R.utils::printf("Suggested knot count: %d\n", knotcnt_suggestion$nknots)
    print_knots(extract_knots(ns_mod))
    R.utils::printf("\n")
  }

  # Natural splines with freely placed knots
  cladina_res <- choose_splines(dataset, !!dependent, !!independents,
    max_nsknots, icr_fn)

  if (verbose) {
    R.utils::printf("%s\n%s: %f\n",
      ret_desc[["ns_nu"]], score_type, cladina_res$score)
    print_knots(cladina_res$knots)
    R.utils::printf("\n")
  }

  if ((mfp_score <= ns_score) && (mfp_score <= cladina_res$score)) {
    ret <- list(model = mfp_mod, type = "mfp", score = mfp_score)
  } else if (ns_score <= cladina_res$score) {
    ret <- list(model = ns_mod, type = "ns", score = ns_score)
  } else {
    ret <- list(model = cladina_res$model, type = "ns_nu",
      score = cladina_res$score)
  }

  if (verbose) {
    R.utils::printf("Chosen model type:\n%s\n", ret_desc[[ret$type]])
  }

  return(ret)
}

#region code for debugging
# main <- function() {
#   library(tidyverse)
#   library(tidyr)
#   library(mfp)
#   library("cladina")

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

#   cladina_res <- choose_model(d, Dependent, Independent, AIC)
# }
#endregion