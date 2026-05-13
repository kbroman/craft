#' Get rounded player position
#'
#' Get rounded player position
#'
#' @param player_id Integer giving the ID of a player
#'
#' @param tile If TRUE, truncate the result to integers
#'
#' @details This is just like [miner::getPlayerPos()] but with `tile=TRUE`
#' being the default.
#'
#' @author Felix Ling
#'
#' @seealso [miner::getPlayerPos()]
#'
#' @importFrom miner getPlayerPos
#' @export
#'
#' @examples
#' \dontrun{
#' mc_connect()
#' whereami()
#' }

whereami <-
    function(player_id = NULL, tile=FALSE)
{
    miner::getPlayerPos(player_id, tile=TRUE)
}
