# render a bitmap in minecraft
#
# bitmap is matrix of 0's and 1's
# bottomleft is a vector of length 3 giving position of lower left corner of first letter
#      (x=north/south, y=up/down, z=east/west)
# id, style are single integers for block type
# dir = direction that the word runs
# top = direction that the top of the word points
#   Note that not all (dir,top) pairs are valid
#   For example, if dir=north or south, top can't be north or south
#' @importFrom miner setBlock
render_bitmap <- function(bitmap, bottomleft, id, style=0,
                          dir=c("north", "south", "east", "west", "up", "down"),
                          top=c("up", "down", "east", "west", "north", "south"))
{
    dir <- match.arg(dir)
    top <- match.arg(top)
    x0 <- bottomleft[1]
    y0 <- bottomleft[2]
    z0 <- bottomleft[3]

    # check that (dir,top) pair is valid
    dir_pairs <- list(c("north", "south"),
                      c("east", "west"),
                      c("up", "down"))
    for(i in seq_along(dir_pairs)) {
        if(dir %in% dir_pairs[[i]] && top %in% dir_pairs[[i]])
            stop("Can't have dir==\"", dir, '" and top=="', top, '"')
    }

    # east =  positive x   west =  negative x
    # up =    positive y   down =  negative y
    # south = positive z   north = negative z

    # multiplier about direction we're going
    if(dir %in% c("east", "south", "up")) fmult <- 1
    else fmult <- -1
    if(top %in% c("east", "south", "up")) tmult <- -1
    else tmult <- +1

    ns <- c("north", "south")
    ew <- c("east", "west")
    ud <- c("up", "down")

    x <- x0
    y <- y0
    z <- z0
    for(i in 1:nrow(bitmap)) { # top -> bottom
        for(j in 1:ncol(bitmap)) { # left -> right
            if(bitmap[i,j]) {
                if(dir %in% ns) {
                    z <- z0 + fmult*(j-1)
                }
                else if (dir %in% ew) {
                    x <- x0 + fmult*(j-1)
                }
                else { # up/down
                    y <- y0 + fmult*(j-1)
                }

                if(top %in% ns) {
                    z <- z0 + tmult*(i-1 - nrow(bitmap))
                }
                else if(top %in% ew) {
                    x <- x0 + tmult*(i-1 - nrow(bitmap))
                }
                else { # up/down
                    y <- y0 + tmult*(i-1 - nrow(bitmap))
                }

                miner::setBlock(x, y, z, id, style)
            }
        }
    }
}
