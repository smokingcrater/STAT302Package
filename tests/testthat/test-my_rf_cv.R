test_that("my_rf_cv() invalid input for parameter k throws error", {
  expect_error(my_rf_cv("five"))
  expect_error(my_rf_cv(TRUE, feedback = FALSE))
})
test_that("my_rf_cv() invalid input for parameter feedback throws error", {
  expect_error(my_rf_cv(feedback = "FALSE"))
  expect_error(my_rf_cv(5, feedback = "TRUE"))
})
test_that("my_rf_cv() returns a double", {
  expect_type(my_rf_cv(k = 5, feedback = FALSE), "double")
})
test_that("my_rf_cv() no message for parameter feedback throws error", {
  expect_output(my_rf_cv(feedback = TRUE))
  expect_output(my_rf_cv(k = 5, feedback = TRUE))
})
