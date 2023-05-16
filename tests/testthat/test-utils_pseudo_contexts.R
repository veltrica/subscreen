#
#
#

test_that("Dimension Check", {
  expect_equal(dim(results_factorial_false$sge), c(171,12))
  expect_equal(dim(results_factorial_true$sge), c(171,18))

  expect_equal(dim(pseudo_contexts(results_factorial_false$sge, "HR.os", results_factorial_false$factors)), c(171,15))
  expect_equal(dim(pseudo_contexts(results_factorial_false$sge, "HR.os", results_factorial_false$factors)), c(171,18))

  expect_equal(dim(pseudo_contexts(results_factorial_false$sge, "HR.os", results_factorial_false$factors)), c(171,15))
  expect_equal(dim(pseudo_contexts(results_factorial_false$sge, "HR.os", results_factorial_false$factors)), c(171,18))
})

# test_that("Correct status", {
#   pseudo_contexts(results_factorial_false$sge, "HR.os", results_factorial_false$factors)
# })
