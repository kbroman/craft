context("check_dirtop")

test_that("check_dirtop works", {

    dirs <- c("north", "south", "east", "west", "up", "down")
    dirpair <- list(c("east", "west"), c("north", "south"), c("up", "down"))

    for(dir in dirs)
        expect_error(check_dirtop(dir, dir))

    for(d in dirpair) {
        expect_error(check_dirtop(d[1], d[2]))
        expect_error(check_dirtop(d[2], d[1]))
    }

    for(i in seq_along(dirpair)) {
        for(dir in dirpair[[i]]) {
            for(top in unlist(dirpair[-i])) {
                expect_silent(check_dirtop(dir, top))
            }
        }
    }

})
