context("tick2char")

test_that("tick2char works", {

    expect_equal(tick2char(1:10), as.character(1:10))

    expect_equal(tick2char(c(0.2, 0.4, 0.6, 0.8, 1)), c("0.2", "0.4", "0.6", "0.8", "1.0"))

    expect_equal(tick2char(c(0, 0.1, 0.3, 0.5)), c("0.0", "0.1", "0.3", "0.5"))

    expect_equal(tick2char(c(0, 0.01, 0.02)), c("0.00", "0.01", "0.02"))

})
