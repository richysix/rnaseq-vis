test_that("max scale", {
  test_m <- matrix(data = c(2,4,6,8,10, 3,6,9,12,15, 0,0,0,0,0, 1,1,1,1,10), ncol = 5, byrow = TRUE)
  exp_m <- matrix(data = c(0.2,0.4,0.6,0.8,1, 0.2,0.4,0.6,0.8,1, 0,0,0,0,0, 0.1,0.1,0.1,0.1,1), ncol = 5, byrow = TRUE)
  expect_equal(max_scale(test_m), exp_m)
})
