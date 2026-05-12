#' Render R logo in minecraft
#'
#' Render the R logo in minecraft
#'
#' @param bottomleft Bottom left position (
#'
#' @param height Height of R logo in blocks
#'
#' @param width Width of R logo in blocks
#' (if not provided, determined to preserve aspect ratio
#'
#' @param dir Which direction should the logo go?
#'
#' @importFrom imager resize
#' @importFrom miner setBlock
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(miner)
#' mc_connect()
#' pos <- getPlayerPos(getPlayerIds()[1])
#' buildRlogo(pos + c(5, 10, 5))
#' }

buildRlogo <-
    function(bottomleft, height=80, width=NULL,
             dir=c("north", "south", "east", "west"))
{
    dir <- match.arg(dir)

    # download file
    data(Rlogo)

    # resize
    if(is.null(width)) Rlogo <- imager::resize(Rlogo, height, round(ncol(Rlogo)/nrow(Rlogo)*height))
    else Rlogo <- imager::resize(Rlogo, height, width)

    Rlogo <- Rlogo[,,1,1]

    blue <- data.frame(name="Blue Wool", id=35, style=11) # miner::find_item("Blue Wool")
    gray <- data.frame(name="Light Gray Wool", id=35, style=8) # miner::find_item("Light Gray Wool")

    # east =  positive x   west =  negative x
    # south = positive z   north = negative z
    x <- x0 <- bottomleft[1]
    y <- y0 <- bottomleft[2]
    z <- z0 <- bottomleft[3]

    for(i in 1:nrow(Rlogo)) {
        for(j in 1:ncol(Rlogo)) {
            if(Rlogo[i,j] == 2 || Rlogo[i,j] == 3) {
                if(dir == "north") {
                    z <- z0 - (i-1)
                } else if(dir == "south") {
                    z <- z0 + (i-1)
                } else if(dir == "east") {
                    x <- x0 + (i-1)
                } else {
                    x <- x0 - (i-1)
                }

                y <- y0 + nrow(Rlogo) - (j-1)
            }

            if(Rlogo[i,j]==2)
                miner::setBlock(x, y, z,
                                blue[2], blue[3])
            if(Rlogo[i,j] == 3)
                miner::setBlock(x, y, z,
                                gray[2], gray[3])
        }
    }
}
