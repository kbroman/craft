#' Render R logo in minecraft
#'
#' Render the R logo in minecraft
#'
#' @param lowerleft Lower left position (
#'
#' @param height Height of R logo in blocks
#'
#' @param width Width of R logo in blocks
#' (if not provided, determined to preserve aspect ratio
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
    function(lowerleft, height=80, width=NULL)
{
    # download file
    data(Rlogo)

    # resize
    if(is.null(width)) Rlogo <- imager::resize(Rlogo, height, round(ncol(Rlogo)/nrow(Rlogo)*height))
    else Rlogo <- imager::resize(Rlogo, height, width)

    Rlogo <- Rlogo[,,1,1]

    blue <- data.frame(name="Blue Wool", id=35, style=11) # miner::find_item("Blue Wool")
    gray <- data.frame(name="Light Gray Wool", id=35, style=8) # miner::find_item("Light Gray Wool")

    for(i in 1:nrow(Rlogo)) {
        for(j in 1:ncol(Rlogo)) {
            if(Rlogo[i,j] == 2)
                miner::setBlock(lowerleft[1]+(nrow(Rlogo)-i),
                                lowerleft[2]+(ncol(Rlogo)-j),
                                lowerleft[3],
                                blue[2], blue[3])
            if(Rlogo[i,j] == 3)
                miner::setBlock(lowerleft[1]+(nrow(Rlogo)-i),
                                lowerleft[2]+(ncol(Rlogo)-j),
                                lowerleft[3],
                                gray[2], gray[3])
        }
    }

}
