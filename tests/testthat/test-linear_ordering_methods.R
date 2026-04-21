test_that("linear_ordering_methods work correctly and alt.row is sequence", {
  set.seed(42)
  d <- matrix(rpois(9, 5), nrow = 3, ncol = 3)
  w <- c(1, 1, 2)
  i <- c('+', '-', '+')
  
  res_sum <- sum_of_ranks(d, w, i)
  expect_equal(res_sum$alt.row, 1:3)
  
  res_std <- standardized_sums(d, w, i)
  expect_equal(res_std$alt.row, 1:3)
  
  res_hel <- hellwig(d, w, i)
  expect_equal(res_hel$alt.row, 1:3)
  
  res_top <- topsis(d, w, i)
  expect_equal(res_top$alt.row, 1:3)
})
