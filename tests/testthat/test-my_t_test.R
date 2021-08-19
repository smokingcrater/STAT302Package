testdata <- rnorm(100, mean = 0, sd = 1)
test_that("my_t.test() non-numeric input for parameter x or mu throws error", {
  expect_error(my_t.test("one"))
  expect_error(my_t.test(c("one", "two")))
  expect_error(my_t.test(testdata, mu = "zero"))
})
test_that("my_t.test() invalid input for parameter alternative throws error", {
  expect_error(my_t.test(testdata, alternative = "unsure"))
})
test_that("my_t.test() returns a list", {
  expect_type(my_t.test(testdata), "list")
})
test_that("my_t.test() and t.test() return identical values for same inputs", {
  expect_equal(my_t.test(testdata, alternative = "two.sided", mu = 0)$p_val,
               t.test(testdata, alternative = "two.sided", mu = 0)$p.value)
})
