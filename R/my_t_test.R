#' t-test
#'
#' This function performs a one sample t-test.
#'
#' @param x Numeric vector of data comprising sample values.
#' @param alternative String specifying the alternative hypothesis, can be
#'   one of three values: less, two.sided, or greater.
#' @param mu The numeric mean of the null hypothesis.
#' @keywords inference, statistical test, t-test
#'
#' @importFrom stats pt sd
#'
#' @return A list comprised of the following: \code{test_stat} (the numeric
#'   test statistic), \code{df} (the degrees of freedom), \code{alternative}
#'   (a string representing the alternative hypothesis), and \code{p_val} (the
#'   numeric p-value).
#'
#' @examples
#' my_t.test(rnorm(100, mean = 0, sd = 1), alternative = "less", mu = 0)
#' my_t.test(rnorm(100, mean = 0, sd = 1), alternative = "greater")
#' my_t.test(rnorm(100, mean = 0, sd = 1))
#'
#' @export
my_t.test <- function(x, alternative = "two.sided", mu = 0) {
  # Verify valid inputs for args 1 and 3.
  if (!(is.numeric(x) & is.numeric(mu))) {
    stop("inputs x and mu must be numeric")
  }

  # Verify valid input for arg 2.
  if (!((identical(alternative,"two.sided") |
         identical(alternative, "less") |
         identical(alternative, "greater")))
  ) {
    stop("alternative parameter must be either two.sided, less, or greater")
  }

  # Calculate the mean.
  my_mean <- mean(x)
  # Calculate the difference from the mean of the null hypothesis.
  my_diff <- my_mean - mu
  # Calculate the standard deviation.
  my_sd <- sd(x)
  # Determine the sample size.
  my_size <- length(x)
  # Calculate the standard error.
  my_se <- my_sd / (sqrt(my_size))
  # Calculate the test statistic.
  my_t <- my_diff / my_se
  # Determine the degrees of freedom.
  my_df <- my_size - 1

  # Determine the p-value associated with the test statistic.
  if  (identical(alternative, "two.sided")) {
    my_p <- stats::pt(abs(my_t), df = my_df, lower.tail = FALSE) * 2
  } else if (identical(alternative, "less")) {
    my_p <- stats::pt(abs(my_t), df = my_df, lower.tail = FALSE)
  } else if (identical(alternative, "greater")) {
    my_p <- stats::pt(abs(my_t), df = my_df, lower.tail = TRUE)
  }

  # Save test statistic, degrees of freedom, alternative hypothesis, and
  # p-value in a list.
  result <- list("test_stat" = my_t,
                 "df" = my_df,
                 "alternative" = alternative,
                 "p_val" = my_p)

  return(result)
}
