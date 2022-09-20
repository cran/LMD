test_that("Find Extrema works", {
  signal=c( 0.841471 ,  0.9092974,  0.14112  , -0.7568025, -0.9589243,
                 -0.2794155,  0.6569866,  0.9893582,  0.4121185, -0.5440211)

  expect_equal(find_extrema(signal)[1], 1)
  expect_equal(find_extrema(signal)[2], 2)
  expect_equal(find_extrema(signal)[3], 5)
  expect_equal(find_extrema(signal)[4], 8)
  expect_equal(find_extrema(signal)[5], 10)
})
