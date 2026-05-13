#' Place a random mixture of blocks in a cuboid
#'
#' Place blocks of a random mixture of types in the cuboid
#' with opposite corners at the positions (x0, y0, z0) and (x1, y1, z1).
#'
#' @param x0 A numeric string with north/south position of one corner
#' @param y0 A numeric string with height of one corner
#' @param z0 A numeric string with east/west position of one corner
#' @param x1 A numeric string with north/south position of opposite corner
#' @param y1 A numeric string with height of opposite corner
#' @param z1 A numeric string with east/west position of opposite corner
#' @param ids Vector of block ids
#' @param styles Vector of block styles (same length as `ids`)
#' @param prob Probabilities for each block type (same length as `ids`)
#' If NULL, they are taken as equally probable.
#'
#' @return None.
#'
#' @author Felix Ling
#'
#' @details This is like [miner::setBlocks()] but placing a random
#' mixture of blocks.
#'
#' @seealso [miner::setBlock()], [miner::setBlocks()], [setBlocksStyle()]
#'
#' @importFrom miner setBlock
#' @export
#'
#' @examples
#' \dontrun{
#' mc_connect()
#'
#' items <- find_item("stained glass$")
#' pos <- whereami()
#' setBlocksMix(pos[1]+2, pos[2], pos[3]+2,
#'              pos[1]+2, pos[2]+5, pos[3]+2,
#'              ids=items[,2], styles=items[,3])
#' }
#'
#' @export

setBlocksMix <- function(x0,y0,z0, x1,y1,z1,
                         ids, styles=NULL, prob=NULL)
{
    x0 <- floor(as.numeric(x0))
    y0 <- floor(as.numeric(y0))
    z0 <- floor(as.numeric(z0))
    x1 <- floor(as.numeric(x1))
    y1 <- floor(as.numeric(y1))
    z1 <- floor(as.numeric(z1))
    id <- floor(as.numeric(id))

    if(is.null(styles)) styles <- rep(0, length(ids))
    if(is.null(prob)) prob <- rep(1, length(ids))/length(ids)
    stopifnot(length(styles) == length(ids))
    stopifnot(length(prob) == length(ids))

    randomi <- sample(seq_along(ids),
                      (abs(x1-x0)+1)*(abs(y1-y0)+1)*(abs(z1-z0)+1),
                      replace=TRUE, prob=prob)

    ids <- ids[randomi]
    styles <- styles[randomi]

    i <- 1
    for(x in x0:x1) {
        for(y in y0:y1) {
            for(z in z0:z1) {
                setBlock(x, y, z, ids[i], styles[i])
                i <- i+1
            }
        }
    }

}
