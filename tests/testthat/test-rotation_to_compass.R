context("rotation_to_compass")

test_that("rotation_to_compass works", {

    # function to get the result for the the values of n_compass_points
    multi_compass <- function(angle)
        sapply(c(4,8,16), function(ndir) rotation_to_compass(angle, ndir))

    expect_equal(multi_compass(0),    rep("south", 3))
    expect_equal(multi_compass(360),  rep("south", 3))
    expect_equal(multi_compass(-360), rep("south", 3))

    expect_equal(multi_compass(90),     rep("west", 3))
    expect_equal(multi_compass(90+360), rep("west", 3))
    expect_equal(multi_compass(90-360), rep("west", 3))

    expect_equal(multi_compass(180),     rep("north", 3))
    expect_equal(multi_compass(180+360), rep("north", 3))
    expect_equal(multi_compass(180-360), rep("north", 3))

    expect_equal(multi_compass(270),     rep("east", 3))
    expect_equal(multi_compass(270+360), rep("east", 3))
    expect_equal(multi_compass(270-360), rep("east", 3))

    expect_equal(multi_compass(157.3),     c("north", "northwest", "north-northwest"))
    expect_equal(multi_compass(157.3+360), c("north", "northwest", "north-northwest"))
    expect_equal(multi_compass(157.3-360), c("north", "northwest", "north-northwest"))

    expect_equal(multi_compass(300),     c("east", "southeast", "east-southeast"))
    expect_equal(multi_compass(300+360), c("east", "southeast", "east-southeast"))
    expect_equal(multi_compass(300-360), c("east", "southeast", "east-southeast"))

})
