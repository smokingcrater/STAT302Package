test_that("my_t.test() non-numeric input for parameter x or mu throws error", {
  set.seed(666)
  testdata <- rnorm(100, mean = 0, sd = 1)
  expect_error(my_t.test("one"))
  expect_error(my_t.test(c("one", "two")))
  expect_error(my_t.test(testdata, mu = "zero"))
})
test_that("my_t.test() invalid input for parameter alternative throws error", {
  set.seed(666)
  testdata <- rnorm(100, mean = 0, sd = 1)
  expect_error(my_t.test(testdata, alternative = "unsure"))
})
test_that("my_t.test() returns a list", {
  set.seed(666)
  testdata <- rnorm(100, mean = 0, sd = 1)
  expect_type(my_t.test(testdata), "list")
})
test_that("my_t.test() and t.test() return identical values for same inputs", {
  set.seed(666)
  testdata <- rnorm(100, mean = 1, sd = 1)
  expect_equal(my_t.test(testdata, alternative = "less", mu = 0)$p_val,
               t.test(testdata, alternative = "less", mu = 0)$p.value)
  expect_equal(my_t.test(testdata, alternative = "two.sided", mu = 0)$p_val,
               t.test(testdata, alternative = "two.sided", mu = 0)$p.value)
  expect_equal(my_t.test(testdata, alternative = "greater", mu = 0)$p_val,
               t.test(testdata, alternative = "greater", mu = 0)$p.value)
})
