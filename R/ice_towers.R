#' Make a random ice tower at hit locations
#'
#' Make an ice tower of a random height wherever you hit.
#'
#' @param player_id Player's entity ID
#' @param block_id Item ID (id=212 for frosted ice)
#' @param max_height Maximum height of ice tower
#' @param delay Delay (in seconds) between calls to the minecraft server
#'
#' @return None.
#'
#' @seealso [miner::getBlockHits()]
#'
#' @note Only right clicks with an iron sword will work.
#'
#' @importFrom miner setBlocks getBlockHits
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # connect to minecraft
#' miner::mc_connect()
#'
#' # Need to use ctrl-c to stop
#' ice_towers( getPlayerIds()[1] )
#' }


ice_towers <-
    function(player_id, block_id=212, max_height=4, delay = 0.2)
{
    stopifnot(max_height >= 1)

    while(TRUE){
        latest_events <- miner::getBlockHits()
        if(nrow(latest_events) == 0) next

        latest_events <- latest_events[latest_events$player == player_id,, drop=FALSE]

        if(nrow(latest_events) == 0) next

        # find unique rows
        v <- apply(latest_events[,1:3], 1, paste, collapse=":")
        uv <- unique(v)
        m <- match(uv, v)
        latest_events <- latest_events[m, , drop=FALSE]

        for(i in 1:nrow(latest_events)){
            tower_height <- sample(1:max_height, size = 1)
            miner::setBlocks(latest_events$x[i], latest_events$y[i], latest_events$z[i],
                             latest_events$x[i], latest_events$y[i] + tower_height, latest_events$z[i],
                             id=block_id)
        }
        Sys.sleep(delay)}

}
