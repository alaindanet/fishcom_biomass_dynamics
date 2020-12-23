context("mon_premier_test")

test_that("gini computation works", {
  mat_gini1 <- matrix(c(0,0,1,0), nrow = 2)
  expect_equal(compute_gini(mat_gini1), 1)
  mat_gini0 <- matrix(c(1,1,1,1), nrow = 2)
  expect_equal(compute_gini(mat_gini0),0)
})
