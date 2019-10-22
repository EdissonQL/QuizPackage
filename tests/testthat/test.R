context("QuizPackage")

testthat::test_that("Test filename", {
                    expect_equal(make_filename(2015),"accident_2015.csv.bz2")
                    })

