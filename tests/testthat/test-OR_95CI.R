test_that("OR_95CI works", {
  expect_equal(OR_95CI(1,1,0.95,1),"2.7 (2.6, 2.9)")
})
