test_that("is_monotonous works", {
  expect_equal(is_monotonous(1:100), TRUE)
  expect_equal(is_monotonous(100:1), TRUE)
  expect_equal(is_monotonous(sin(1:100)), FALSE)



})
