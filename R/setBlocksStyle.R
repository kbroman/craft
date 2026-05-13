#' Place blocks in a cuboid
#'
#' Place blocks of a single type (specified by `id`) in the cuboid
#' with opposite corners at the positions (x0, y0, z0) and (x1, y1, z1).
#'
#' @param x0 A numeric string with north/south position of one corner
#' @param y0 A numeric string with height of one corner
#' @param z0 A numeric string with east/west position of one corner
#' @param x1 A numeric string with north/south position of opposite corner
#' @param y1 A numeric string with height of opposite corner
#' @param z1 A numeric string with east/west position of opposite corner
#' @param id Block id
#' @param style Block style
#'
#' @return None.
#'
#' @details This is just like [miner::setBlocks()] but with an
#' added `style` argument.
#'
#' @seealso [miner::setBlock()], [miner::setBlocks()]
#'
#' @importFrom miner setBlock
#'
#' @examples
#' \dontrun{
#' mc_connect()
#'
#' item <- find_item("Blue Stained Glass")
#' pos <- whereami()
#' setBlocksStyle(pos[1]+2, pos[2], pos[3]+2,
#'           pos[1]+2, pos[2]+5, pos[3]+2,
#'           id=item[2], style=item[3])
#' }
#'
#' @export

setBlocksStyle <- function(x0,y0,z0, x1,y1,z1,
                           id, style=0)
{
    x0 <- floor(as.numeric(x0))
    y0 <- floor(as.numeric(y0))
    z0 <- floor(as.numeric(z0))
    x1 <- floor(as.numeric(x1))
    y1 <- floor(as.numeric(y1))
    z1 <- floor(as.numeric(z1))
    id <- floor(as.numeric(id))

    for(x in x0:x1) {
        for(y in y0:y1) {
            for(z in z0:z1) {
                setBlock(x, y, z, id, style)
            }
        }
    }

}
