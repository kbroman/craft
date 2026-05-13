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
#' @param blue_id Block ID for blue blocks
#' @param blue_style Block style for blue blocks
#' @param gray_id Block ID for gray blocks
#' @param gray_style Block style for gray blocks
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
#'
#' # used stained glass
#' blue_glass <- find_item("Blue Stained Glass")
#' gray_glass <- find_item("Gray Stained Glass")
#' buildRlogo(pos + c(5, 10, 5),
#'            blue_id=blue_glass[2], blue_style=blue_glass[3],
#'            gray_id=gray_glass[2], gray_style=gray_glass[3])
#' }

buildRlogo <-
    function(bottomleft, height=80, width=NULL,
             dir=c("north", "south", "east", "west"),
             blue_id=35, blue_style=11, gray_id=35, gray_style=8)
{
    dir <- match.arg(dir)

    # download file
    data(Rlogo)

    # resize
    if(is.null(width)) Rlogo <- imager::resize(Rlogo, height, round(ncol(Rlogo)/nrow(Rlogo)*height))
    else Rlogo <- imager::resize(Rlogo, height, width)

    Rlogo <- Rlogo[,,1,1]

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
                                blue_id, blue_style)
            if(Rlogo[i,j] == 3)
                miner::setBlock(x, y, z,
                                gray_id, gray_style)
        }
    }
}
