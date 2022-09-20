test_that("moving_average_smooth works", {
  signal = c(
    0.8753842000000001,
    0.4252853750000001,
    -0.02481344999999996,
    -0.02481344999999996,
    -0.004798249999999976,
    0.015216950000000007,
    0.015216950000000007,
    0.11894274999999999,
    0.22266854999999997,
    0.22266854999999997
  )
  window = 1
  temp_var=moving_average_smooth(signal=signal,window=window)


testthat::expect_lt(temp_var[1], 0.7254)
testthat::expect_gt(temp_var[1], 0.72)
})
