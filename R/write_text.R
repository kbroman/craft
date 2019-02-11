#' Write text in minecraft
#'
#' Write some text in minecraft, using blocks
#'
#' @param text A character string to be written in blocks
#' @param lowerleft Vector of length 3 representing the location for the lower-left corner for the text.
#' @param font Font size to use
#' @param id Block type to use to write the text
#' @param style Block style to use to write the text
#' @param dir Direction the text should run
#' @param top Direction that the top of the text should point
#'
#' @return None
#'
#' @examples
#' \dontrun{
#' library(miner)
#' v <- getPlayerPos()
#' write_text("Hello!", v+c(5,3,0), font="4x8")
#' }
#'
#' @export
write_text <- function(text,
                       lowerleft,
                       font=c("4x5", "4x6", "4x8", "6x6", "8x6", "6x8", "8x8",
                              "4x12", "6x12", "8x12", "8x16", "16x16"),
                       id=1,
                       style=0,
                       dir=c("north", "south", "east", "west", "up", "down"),
                       top=c("up", "east", "west", "north", "south", "down"))
{
    font <- match.arg(font)
    dir <- match.arg(dir)
    top <- match.arg(top)

    bmp <- char2bitmap(text, font)
    render_bitmap(bmp, lowerleft, id, style, dir, top)

}

# get bitmap for a given character
#' @importFrom utils data
char2bitmap <- function(char,
                        font=c("4x5", "4x6", "4x8", "6x6", "8x6", "6x8", "8x8",
                               "4x12", "6x12", "8x12", "8x16", "16x16"))
{
    font <- match.arg(font)

    if(length(char) > 1) {
        return(lapply(char, char2bitmap, font))
    }
    if(nchar(char) > 1) {
        result <- lapply(strsplit(char, "")[[1]], char2bitmap, font)
        return(do.call("cbind", result))
    }

    library(craft) # force load package
    utils::data(font_sets, package="craft", envir=parent.frame())
    chars <- font_sets[[font]]$charset
    png <- font_sets[[font]]$png

    chars <- matrix(unlist(strsplit(chars, "")), ncol=length(chars))

    d <- dim(png)
    rows <- seq(1, d[1]+1, by=d[1]/nrow(chars))
    cols <- seq(1, d[2]+1, by=d[2]/ncol(chars))

    # row column coordinates
    wh <- which(chars==char, arr.ind=TRUE)
    if(length(wh) == 0) { # use a space instead
        warning('Character "', char, '" is not in the ', font, ' font')
        wh <- which(chars==" ", arr.ind=TRUE)
    }
    if(length(wh) > 2) wh <- wh[1,]

    rows <- rows[wh[1]]:(rows[wh[1]+1]-1)
    cols <- cols[wh[2]]:(cols[wh[2]+1]-1)

    round(t(1-png[rows,cols]))
}

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
    check_dirtop(dir, top)

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

# check that dir and top are compatible
check_dirtop <- function(dir, top)
{
    dirs <- list(c("east", "west"),
                 c("north", "south"),
                 c("up", "down"))

    for(d in dirs) {
        if(dir %in% d && top %in% d)
            stop("If dir is ", dir, ", top can't be ", top)
    }
}
