#' Function assessing different types of regression models, returning the one
#' giving the best results according to an information criterion
#'
#' @param dataset The data frame
#' @param dependent The dependent variable in the formula
#' @param independents The independent variables in the formula
#' @param icr_fn The information criterion function for comparing different
#' models with different degress for freedom or knots (default BIC)
#' @param cost_fn The criterion used to choose which knots to remove, passed to
#' the function choose_removal. Defaults to AIC.
#' @param fp_alpha The relax factor for multivariate fractional polynomials
#' @param max_nsknots The max number of knots for natural splines (default 7)
#' @param max_fp_df The max degrees of freedom for fractional polynomials
#' (default 7)
#' @param verbose Verbose output, default TRUE
#' @param boundary_knots The boundary knot placements for natural cubic splines
#' or NA if not specified
#' @return A list with named elements, such as 'model', 'type', 'score'. The
#' function returns a list with named elements and sublists, see examples for
#' full overview of the returned values.
#' @importFrom stats glm
#' @importFrom mfp fp
#' @export
#' @examples
#' my_model <- choose_model(d, y, x)$model
#' result <- choose_model(d, y, x, icr_fn = AIC, verbose = FALSE)
#'
#' ret <- choose_model(d, y, x)
#'
#' ret$labels[[ret$type]] # Human readable name of chosen model type
#' ret[[ret$type]]        # Gives more values for the chosen model if available
#'
#' ret$model      # The chosen model
#' ret$score      # The chosen model's score
#' ret$type       # The type of model chosen as a string
#' ret$score_name # The type of score used as a string
#' ret$score_fn   # The function used for scores
#'
#' ret$labels     # Description string for the types of models
#'
#' ret$mfp        # Multivariate fractional polynomial
#' ret$mfp$model  # The model
#' ret$mfp$score  # The score
#'
#' ret$ns         # Natural splines from quantiles
#' ret$ns$model   # The model
#' ret$ns$score   # The score
#' ret$ns$knot_cnt_arg      # The number of knots (df - 1) as input argument
#' ret$ns$knot_cnt_distinct # The number of distinct placements in the result
#' ret$ns$knot_placements   # Knots and boundary knots
#' ret$ns$knot_placements$knots           # The knot placements as a list
#' ret$ns$knot_placements$Boundary.knots  # The boundary knots as a list
#'
#' ret$ns_nu         # Natural splines non-uniform placements
#' ret$ns_nu$model   # The model
#' ret$ns_nu$score   # The score
#' ret$ns_nu$knot_cnt_distinct # The number of distinct placements in the result
#' ret$ns_nu$knot_placements   # Knots and boundary knots
#' ret$ns_nu$knot_placements$knots          # The knot placements as a list
#' ret$ns_nu$knot_placements$Boundary.knots # The boundary knots as a list
choose_model <- function(dataset,
                        dependent,
                        independents,
                        icr_fn = stats::BIC,
                        cost_fn = stats::AIC,
                        fp_alpha = NA,
                        max_nsknots = 7,
                        max_fp_df = 4,
                        verbose = TRUE,
                        boundary_knots = NA) {
  dependent <- rlang::enquo(dependent)
  independents <- rlang::enquo(independents)

  score_type <- NULL
  if (missing(icr_fn)) {
    icr_fn <- stats::BIC
    score_type <- "BIC"
  } else {
    score_type <- deparse(substitute(icr_fn))
  }
  if (missing(cost_fn)) cost_fn <- stats::AIC
  if (missing(fp_alpha)) fp_alpha <- NA
  if (missing(max_nsknots)) max_nsknots <- 7
  if (missing(max_fp_df)) max_fp_df <- 4
  if (missing(verbose)) verbose <- TRUE
  if (missing(boundary_knots)) boundary_knots <- NA

  ret_desc <- list(
    "mfp" = "Multivariate fractional polynomials",
    "ns_nu" = "Natural splines with freely placed knots",
    "ns" = "Natural splines with knots placed at quantiles")
  ret <-
    list(labels = ret_desc, score_fn = icr_fn, score_name = score_type)

  independents_str <- sub("~", "", deparse(independents))

  # Multivariate fractional polynomials (move to separate func)
  fp_formula <- stats::formula(paste0(
    rlang::as_name(dependent),
    " ~ fp(",
    independents_str,
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

  ret <- append(ret, list(mfp = list(model = mfp_mod, score = mfp_score)))

  if (verbose) {
    R.utils::printf("-----------------------------------------------------\n")
    R.utils::printf("%s\n%s: %f\n\n", ret_desc[["mfp"]], score_type, mfp_score)
  }

suppressWarnings({
    # Natural splines with knots distanced by equally sized bins (quantiles)
    knotcnt_suggestion <-
      suggest_knotcount(dataset, !!dependent, !!independents, max_nsknots,
        icr_fn = icr_fn, boundary_knots = boundary_knots)
    ns_mod <- model_by_count(dataset, !!dependent, !!independents,
      knotcnt_suggestion$nknots, boundary_knots = boundary_knots)
    ns_score <- icr_fn(ns_mod)

    extracted_knots <- extract_knots(ns_mod)
    ret <-
      append(ret, list(ns =
        list(model = ns_mod,
          score = ns_score,
          knot_cnt_arg = knotcnt_suggestion$nknots,
          knot_cnt_distinct = length(extracted_knots$knots),
          knot_placements = extracted_knots)))

    if (verbose) {
      R.utils::printf("%s\n%s: %f\n", ret_desc[["ns"]], score_type, ns_score)
      R.utils::printf("Suggested knot count: %d\n", knotcnt_suggestion$nknots)
      print_knots(extract_knots(ns_mod))
      R.utils::printf("\n")
    }

    # Natural splines with freely placed knots
    cladina_res <- choose_splines(dataset, !!dependent, !!independents,
      max_nsknots, icr_fn = icr_fn, cost_fn = cost_fn,
      boundary_knots = boundary_knots)

    ret <-
      append(ret, list(ns_nu =
        list(model = cladina_res$model,
          score = cladina_res$score,
          knot_cnt_distinct = length(cladina_res$knots$knots),
          knot_placements = cladina_res$knots)))

    if (verbose) {
      R.utils::printf("%s\n%s: %f\n",
        ret_desc[["ns_nu"]], score_type, cladina_res$score)
      print_knots(cladina_res$knots)
      R.utils::printf("\n")
    }

    if ((mfp_score <= ns_score) && (mfp_score <= cladina_res$score)) {
      ret <- append(ret, list(model = mfp_mod, type = "mfp", score = mfp_score))
    } else if (ns_score <= cladina_res$score) {
      ret <- append(ret, list(model = ns_mod, type = "ns", score = ns_score))
    } else {
      ret <- append(ret, list(model = cladina_res$model, type = "ns_nu",
        score = cladina_res$score))
    }

    if (verbose) {
      R.utils::printf("Chosen model type:\n%s\n", ret_desc[[ret$type]])
    }
  })

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

#   ret <- choose_model(d, Dependent, Independent, AIC, verbose = FALSE)

# #  print(ret$mfp)        # Multivariate fractional polynomial
#   print(ret$mfp$model)  # The model
#   print(ret$mfp$score)  # The score

# #  print(ret$ns)         # Natural splines from quantiles
#   print(ret$ns$model)   # The model
#   print(ret$ns$score)   # The score
#   print(ret$ns$knot_cnt_arg)      # The number of knots (df - 1) as input argument
#   print(ret$ns$knot_cnt_distinct) # The number of distinct placements in the result
# #  print(ret$ns$knot_placements)   # Knots and boundary knots
#   print(ret$ns$knot_placements$knots)           # The knot placements as a list
#   print(ret$ns$knot_placements$Boundary.knots) # The boundary knots as a list

#  # print(ret$ns_nu)         # Natural splines non-uniform placements
#   print(ret$ns_nu$model)   # The model
#   print(ret$ns_nu$score)   # The score
#   print(ret$ns_nu$knot_cnt_distinct) # The number of distinct placements in the result
# #  print(ret$ns_nu$knot_placements)   # Knots and boundary knots
#   print(ret$ns_nu$knot_placements$knots)          # The knot placements as a list
#   print(ret$ns_nu$knot_placements$Boundary.knots) # The boundary knots as a list

# }
#endregion
