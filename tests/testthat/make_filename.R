test_that("make_filename generates the file name", {
  testthat::expect_equal(make_filename(2015), "accident_2015.csv.bz2")
})
