test_that("my_lm() non-data.frame input for parameter data throws error", {
  expect_error(my_lm(mpg ~ hp + wt, data = "data"))
  expect_error(my_lm(mpg ~ hp + wt, data = 3, pp = 5))
})
test_that("my_lm() non-numeric input for parameter pp throws error", {
  expect_error(my_lm(mpg ~ hp + wt, data = mtcars, pp = "five"))
})
test_that("my_lm() parameter pp < 0 or > 8 throws error", {
  expect_error(my_lm(mpg ~ hp + wt, data = mtcars, pp = -1))
  expect_error(my_lm(mpg ~ hp + wt, data = mtcars, pp = 9))
})
