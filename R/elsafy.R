#' Give a player Elsa powers
#'
#' Give a player Elsa powers: when she walks on water, it turns to ice.
#'
#' @param player_id Player's entity ID
#' @param water Vector of item IDs for different kinds of water
#' @param ice Item ID for ice
#' @param delay Delay (in seconds) between calls to the minecraft server
#'
#' @return None.
#'
#' @examples
#' \dontrun{
#' # Runs in an infinite loop; press ctrl-C to stop.
#' elsafy(215)
#' }
#'
#' @seealso [de_elsafy()]
#' @importFrom miner getPlayerPos getBlocks setBlock
#' @export
elsafy <- function(player_id=NULL, water=c(8, 9), ice=174, delay=0.02)
{
    while(TRUE) {
        pos <- miner::getPlayerPos(player_id)
        block <- miner::getBlocks(pos[1]-1, pos[2]-1, pos[3]-1,
                                  pos[1]+1, pos[2]-1, pos[3]+1)
        for(i in -1:1) {
            for(j in -1:1) {
                if(block[i+2,1,j+2] %in% water)
                    miner::setBlock(pos[1]+i, pos[2]-1, pos[3]+j, ice)
            }
        }

        Sys.sleep(delay)
    }
}

#' Give a player reverse Elsa powers
#'
#' Give a player revsese Elsa powers, to clean up after running [elsafy()]: when she walks over ice, it turns back to water.
#'
#' @param player_id Player's entity ID
#' @param water Item IDs for water
#' @param ice Item ID for ice
#' @param delay Delay (in seconds) between calls to the minecraft server
#'
#' @return None.
#'
#' @examples
#' \dontrun{
#' # Runs in an infinite loop; press ctrl-C to stop.
#' de_elsafy(215)
#' }
#'
#' @seealso [elsafy()]
#' @export
de_elsafy <- function(player_id=NULL, water=8, ice=174, delay=0.05)
{
    # range of blocks around Elsa to be cleared
    d <- list(-1:1, -5:0, -1:1)

    while(TRUE) {
        pos <- miner::getPlayerPos(player_id)
        block <- miner::getBlocks(pos[1]+min(d[[1]]), min(pos[2]+d[[2]]), min(pos[3]+d[[3]]),
                                  pos[1]+max(d[[1]]), max(pos[2]+d[[2]]), max(pos[3]+d[[3]]))
        for(i in seq_along(d[[1]])) {
            for(j in seq_along(d[[2]])) {
                for(k in seq_along(d[[3]])) {
                    if(block[i,j,k] == ice)
                        miner::setBlock(pos[1]+d[[1]][i],
                                        pos[2]+d[[2]][j],
                                        pos[3]+d[[3]][k],
                                        water)
                }
            }
        }
    }
}
