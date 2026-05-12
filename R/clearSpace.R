#' Clear out a cube around a player
#'
#' Clear out a cube around a player
#'
#' @param length length of each side of the cube to be cleared
#' @param player_id player's entity id
#'
#' @importFrom miner getPlayerPos setBlocks
#' @export
#'
#' @seealso [miner::setBlocks()], [miner::getPlayerPos()]

#' @examples
#' \dontrun{
#' id <- getPlayerIds()[1]
#' clearSpace(5, id)
#' }
clearSpace <- function (length, player_id = NULL) {
   pos <- miner::getPlayerPos(player_id, tile = TRUE)

   # fill cube with air (block id 0)
   miner::setBlocks (pos[1] - length/2, pos[2],          pos[3] - length/2,
                     pos[1] + length/2, pos[2] + length, pos[3] + length/2, 0)
}
