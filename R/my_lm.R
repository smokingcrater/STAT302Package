#' Linear model
#'
#' This function fits a linear model.
#'
#' @param formula A formula class object (same as \code{lm()}).
#' @param data A data frame.
#' @param pp A numeric specifying the scientific notation precision for the
#'   values in the returned table.
#' @keywords regression
#'
#' @importFrom stats pt sd
#'
#' @return A table containing the following cols for each coefficient:
#'   `Estimate` (expected change in the response due to a unit change in the
#'   feature), `Std. Error` (standard error of the estimate), `t value`
#'   (t-statistic), and `Pr(>|t|)` (p-value).
#'
#' @examples
#' my_lm(mpg ~ hp + wt, data = mtcars, pp = 5)
#' my_lm(mpg ~ hp + wt, data = mtcars)
#'
#' @export
my_lm <- function(formula, data, pp = 0) {
  # Verify valid inputs for args 1 and 3.
  if (!is.data.frame(data)) {
    stop("parameter \"data\" must be a data frame")
  }

  if (!is.numeric(pp)) {
    stop("parameter \"pp\" must be numeric")
  } else if (pp < 0 | pp > 8) {
    stop("parameter \"pp\" must be between 0 and 8")
  }

  # Extract the model matrix X.
  my_x <- stats::model.matrix(formula, data)
  # Extract the model frame object (required below by Y).
  my_frame <- stats::model.frame(formula, data)
  # Extract a model frame object.
  my_y <- stats::model.response(my_frame)

  # Solve for linear regression coefficients.
  my_coefficients <- solve(t(my_x) %*% my_x) %*% t(my_x) %*% my_y

  # Degrees of freedom: sample size - # of covariates (including intercept).
  my_df <- nrow(my_x) - ncol(my_x)

  # Calculate variance.
  my_variance <- 0
  for (i in 1:nrow(my_x)) {
    my_variance <- my_variance +
      ((my_y[i] - (my_x[i, ] %*% my_coefficients)) ^ 2) / my_df
  }
  # Convert variance to a scalar.
  my_variance <- c(my_variance)

  # Calculate standard error.
  my_se <- sqrt(diag(my_variance * solve(t(my_x) %*% my_x)))

  # Calculate t-statistic.
  my_t <- my_coefficients / my_se

  # Calculate two-sided p-value.
  my_p <- pt(abs(my_t), df = my_df, lower.tail = FALSE) * 2

  # Package the outputs in a table.
  my_result <- as.table(cbind(my_coefficients, my_se, my_t, my_p))

  # Create column headers for the table.
  colnames(my_result) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")

  # Scientific notation used by default for precision.  However, pretty print
  # can be specified to more closely match output of summary(lm()).
  if (pp > 0) {
    # Iterate over rows.
    for (i in 1:nrow(my_result)) {
      # Iterate over cols.
      for (j in 1:ncol(my_result)) {
        my_result[i, j] <- round(my_result[i, j], pp)
      }
    }
  }
  return(my_result)
}
