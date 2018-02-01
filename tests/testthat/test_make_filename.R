#test_that("make_filename generates the file name", {
#  testthat::expect_equal(make_filename(2015), "accident_2015.csv.bz2")
#})



test_that(".csv data files are available", {
  testthat::expect_equal(list.files(system.file("extdata", package = "fars")),
                       c("accident_2013.csv.bz2",
                         "accident_2014.csv.bz2",
                         "accident_2015.csv.bz2"))
})
