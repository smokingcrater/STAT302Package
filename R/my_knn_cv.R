#' k-Nearest Neighbors Cross-Validation
#'
#' This function applies cross-validation to a nearest neighbor algorithm.
#'
#' @param train A data frame containing the covariates (x).
#' @param cl A vector containing the true classification of the training
#'   data (y).
#' @param k_nn A numeric specifying the number of neighbors.
#' @param k_cv A numeric specifying the number of folds.
#' @param feedback A boolean for debugging feedback.
#' @keywords prediction
#'
#' @importFrom stats pt sd
#'
#' @return A list comprised of two components:  \code{class} (a vector of
#'   predicted classes for all observations) and \code{cv_err} (a numeric
#'   cross-validation misclassification error calculated by taking the mean
#    of the misclassification errors calculated in each fold).
#'
#' @examples
#' train <- as.data.frame(rbind(iris3[1:25,,1], iris3[1:25,,2], iris3[1:25,,3]))
#' cl <- factor(c(rep("s",25), rep("c",25), rep("v",25)))
#' my_knn_cv(train, cl, k_nn = 1, k_cv = 5, feedback = FALSE)
#' my_knn_cv(train, cl, k_nn = 1, k_cv = 5)
#' my_knn_cv(train, cl, k_nn = 1)
#' my_knn_cv(train, cl)
#'
#' @export
my_knn_cv <- function(train, cl, k_nn = 1, k_cv = 5, feedback = FALSE) {
  # Verify valid inputs for all four parameters.
  if (!(is.data.frame(train) | is.matrix(train))) {
    stop("input train must be a matrix or data frame of training set cases")
  }
  if (!is.factor(cl)) {
    stop("input cl must be a factor of true classifications of training set")
  }
  if (!is.numeric(k_nn)) {
    stop("input k_nn must be a number of neighbors considered")
  }
  if (!is.numeric(k_cv)) {
    stop("input k_cv must be a number of cross validation folds")
  }
  if (!is.logical(feedback)) {
    stop("input feedback must be a Boolean")
  }

  # Generate numbers between 1 and the fold count (k_cv) to represent
  # folds to draw from and randomly mix them up.
  fold <- sample(rep(1:k_cv, length = length(cl)))
  train$fold <- fold

  # Store the predicted classifications and using the same
  # factors specified by the known classifications.
  predictions <- factor(rep(NA, times = length(cl)), levels = levels(cl))
  # Alternatively, clone the known classifications and initialize w/NAs.
  # predictions <- my_cl
  # predictions[!is.na(predictions)] <- NA

  # Store the iterative misclassification errors calculated during each fold.
  misclassification_errors <- rep(NA, times = k_cv)

  # Run nearest neighbor algorithm for each fold.
  for (i in 1:k_cv) {
    # Use data rows not associated with the current fold for training.
    data_train <- train %>% dplyr::filter(fold != i)
    # Use data rows associated with the current fold for testing.
    data_test <- train %>% dplyr::filter(fold == i)
    # Use classifications not associated with the current fold for training.
    classification_train <- cl[fold != i]
    # Use classifications associated with the current fold for testing.
    classification_test <- cl[fold == i]

    # Run nearest neighbor classification.
    nn_result <- class::knn(train = data_train,
                            test = data_test,
                            cl = classification_train,
                            k = k_nn)

    # Record iterative misclassification error.
    misclassification_errors[i] <- mean(classification_test != nn_result)
    # Save predictions for calculating cumulative misclassification error.
    predictions[fold == i] <- nn_result
  }

  # Calculate overall misclassification error by taking the mean of the
  # misclassification errors calculated during each iteration (not as accurate
  # as cumulative unless the data set is perfectly divisible by k_cv).
  error_iterative <- mean(misclassification_errors)

  # Calculate the overall misclassification error by taking the mean of
  # all of the misclassifications across all folds (easier and more accurate).
  error_cumulative <- mean(cl != predictions)

  # Use full data for both the training and test data.
  nn_result <- class::knn(train = train, test = train, cl = cl, k = k_nn)

  if (feedback) {
    cat("iterative misclassification_errors:", misclassification_errors, "\n")
    cat("error_iterative:", error_iterative, "\n")
    cat("error_cumulative:", error_cumulative, "\n")
  }

  # In order to return multiple values, pack them into a list (ain't R cool?).
  return(list("class" = nn_result, "cv_err" = error_iterative))
}
