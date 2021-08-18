#' Random Forest Cross-Validation
#'
#' This function applies cross-validation to a random forest algorithm on the
#'   penguins data set.
#'
#' @param k A numeric specifying the number of folds.
#' @param feedback A boolean for debugging feedback.
#' @keywords prediction
#'
#' @importFrom stats pt sd
#'
#' @return A numeric specifying the average mean squared error (MSE) across
#'   all folds.
#'
#' @examples
#' my_rf_cv(5, TRUE)
#' my_rf_cv(5)
#' my_rf_cv()
#'
#' @export
my_rf_cv <- function(k = 5, feedback = FALSE) {
  # Verify valid input..
  if (!is.numeric(k)) {
    stop("input k must be a number of cross validation folds")
  }

  # Subset columns (classification = y = body_mass_g, covariates = x =
  # bill_length_mm, bill_depth_mm, and flipper_length_mm and eliminate NAs.
  clean_penguins <-
    STAT302Package::my_penguins %>%
    dplyr::select(body_mass_g, bill_length_mm, bill_depth_mm, flipper_length_mm) %>%
    stats::na.omit()

  # Generate numbers between 1 and the fold count (k) to represent
  # folds to draw from and randomly mix them up.
  fold <- sample(rep(1:k, length = nrow(clean_penguins)))

  # Store the predicted classifications.
  predictions <- rep(NA, times = nrow(clean_penguins))

  # Store the mean squared error calculated during each fold.
  mean_squared_errors <- rep(NA, times = k)

  # Run random forest algorithm for each fold.
  for (i in 1:k) {
    # Use data rows not associated with the current fold for training.
    data_train <- clean_penguins %>% dplyr::filter(fold != i)
    # Use data rows associated with the current fold for testing.
    data_test <- clean_penguins %>% dplyr::filter(fold == i)
    # Use data rows associated with the current fold for true body_mass_g.
    body_mass_g_true <- clean_penguins$body_mass_g[fold == i]

    # Run algorithm in randomForest() to train a random forest model
    # w/100 trees to predict body_mass_g using covariates bill_length_mm,
    # bill_depth_mm, and flipper_length_mm.
    rf_model <-
      randomForest::randomForest(body_mass_g ~
                                   bill_length_mm +
                                   bill_depth_mm +
                                   flipper_length_mm,
                                 data = data_train,
                                 ntree = 100)
    # Predict the body_mass_g of this fold.
    prediction <- stats::predict(rf_model, data_test[, -1])

    # Calculate and save the mean squared error as the mean of the square of
    # all of the differences between the predicted value and the truth value
    # in each fold.
    mean_squared_errors[i] <- mean((body_mass_g_true - prediction)^2)
    # Save predictions for calculating cumulative error.
    predictions[fold == i] <- prediction
  }

  # Calculate overall mean squared error by taking the mean of the mean
  # squared errors during each iteration (not as accurate as cumulative
  # unless the data set is perfectly divisible by k).
  mean_squared_error_iterative <- mean(mean_squared_errors)

  # Calculate the overall mean squared error by taking the mean of the square
  # of all differences between the predicted value and the truth value across
  # all folds (easier and more accurate).
  mean_squared_error_cumulative <-
    mean((clean_penguins$body_mass_g - predictions)^2)

  if (feedback) {
    cat("iterative mean_squared_errors:", mean_squared_errors, "\n")
    cat("mean_squared_error_iterative:", mean_squared_error_iterative, "\n")
    cat("mean_squared_error_cumulative:", mean_squared_error_cumulative, "\n")
  }

  return(mean_squared_error_iterative)
}

utils::globalVariables(c("bill_length_mm", "bill_depth_mm", "body_mass_g", "flipper_length_mm"))
