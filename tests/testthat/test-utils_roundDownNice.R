# Test file for roundDownNice
#
# Last update: 17MAY2022

test_that("Check values",{
   expect_equal(roundDownNice(1.3214, nice = c(1, 1.5, 2, 4, 5, 6, 8, 10)), 1)
   expect_equal(roundDownNice(1.6214, nice = c(1, 1.5, 2, 4, 5, 6, 8, 10)), 1.5)
   expect_equal(roundDownNice(999.9999, nice = c(1, 1.5, 2, 4, 5, 6, 8, 10)), 800)
})
