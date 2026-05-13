# versions of functions that take vectors rather than separate values
#
# setBlockV, setBlocksV, setBlocksStyleV, setBlocksMixV
# setPlayerPosV, setPlayerDirectionV

#' setBlock but taking a vector position
#'
#' Place a block at position (x,y,z) by type id
#'
#' @param pos Vector (x,y,z) of position
#' @param id Block ID
#' @param style Block style
#'
#' @return None.
#'
#' @seealso [miner::setBlock()]
#'
#' @examples
#' \dontrun{
#' library(miner)
#' mc_connect()
#' p <- getPlayerPos()
#' setBlockV(p + c(0, 5, 0), 46)
#' }
#'
#' @export
#' @importFrom miner setBlock

setBlockV <- function(pos, id, style=0)
{
    stopifnot(length(pos)==3)
    stopifnot(all(!is.na(pos)))

    miner::setBlock(pos[1], pos[2], pos[3], id, style)
}


#' setBlocks but taking vector positions
#'
#' Place a cuboid of blocks of a single type
#'
#' @param pos0 Vector (x,y,z) of first position
#' @param pos1 Vector (x,y,z) of second position
#' @param id Block ID
#'
#' @return None.
#'
#' @seealso [miner::setBlocks()]
#'
#' @examples
#' \dontrun{
#' library(miner)
#' mc_connect()
#' p <- getPlayerPos()
#' setBlocksV(p + c(0, 1, 0), p + c(0, 5, 0), 46)
#' }
#'
#' @export
#' @importFrom miner setBlocks

setBlocksV <- function(pos0, pos1, id)
{
    stopifnot(length(pos0)==3)
    stopifnot(all(!is.na(pos0)))
    stopifnot(length(pos1)==3)
    stopifnot(all(!is.na(pos1)))

    miner::setBlocks(pos0[1], pos0[2], pos0[3],
                     pos1[1], pos1[2], pos1[3], id)
}


#' setBlocksStyle but taking vector positions
#'
#' Place a cuboid of blocks of a single type, allowing a style parameter
#'
#' @param pos0 Vector (x,y,z) of first position
#' @param pos1 Vector (x,y,z) of second position
#' @param id Block ID
#' @param style Block style
#'
#' @return None.
#'
#' @seealso [setBlocksStyle()]
#'
#' @examples
#' \dontrun{
#' library(miner)
#' mc_connect()
#' p <- getPlayerPos()
#' setBlocksStyleV(p + c(0, 1, 0), p + c(0, 5, 0), 95, 2)
#' }
#'
#' @export

setBlocksStyleV <- function(pos0, pos1, id, style)
{
    stopifnot(length(pos0)==3)
    stopifnot(all(!is.na(pos0)))
    stopifnot(length(pos1)==3)
    stopifnot(all(!is.na(pos1)))

    setBlocksStyle(pos0[1], pos0[2], pos0[3],
                   pos1[1], pos1[2], pos1[3], id, style)
}


#' setBlocksMix but taking vector positions
#'
#' Place a cuboid of blocks of a random mixture of types
#'
#' @param pos0 Vector (x,y,z) of first position
#' @param pos1 Vector (x,y,z) of second position
#' @param ids Vector of block IDs
#' @param styles Vector of block styles
#' @param prob Probabilities for each block type
#'
#' @return None.
#'
#' @seealso [setBlocksMix()]
#'
#' @examples
#' \dontrun{
#' library(miner)
#' mc_connect()
#' p <- getPlayerPos()
#' setBlocksStyleV(p + c(0, 1, 0), p + c(0, 5, 0), 95, 2)
#' }
#'
#' @export

setBlocksMixV <- function(pos0, pos1, ids, styles=NULL, prob=NULL)
{
    stopifnot(length(pos0)==3)
    stopifnot(all(!is.na(pos0)))
    stopifnot(length(pos1)==3)
    stopifnot(all(!is.na(pos1)))

    setBlocksMix(pos0[1], pos0[2], pos0[3],
                 pos1[1], pos1[2], pos1[3], ids, styles, prob)
}

# setPlayerPosV
#' setPlayerPos but taking a vector of positions
#'
#' Move player to position (x,y,z) specified as a vector
#'
#' @param pos Vector (x,y,z) giving position
#' @param player_id Player ID
#' @param tile If TRUE, truncation position to integers
#'
#' @return None.
#'
#' @examples
#' \dontrun{
#' library(miner)
#' mc_connect()
#' pos <- getPlayerPos()
#' setPlayerPosV(pos + c(0, 5, 0))
#' }
#'
#' @export
#' @importFrom miner setPlayerPos
#'
#' @seealso [miner::setPlayerPos()]

setPlayerPosV <-
    function(pos, player_id=NULL, tile=FALSE)
{
    miner::setPlayerPos(pos[1], pos[2], pos[3], player_id, tile)
}


# setPlayerDirectionV
#' setPlayerDirection but taking a vector of positions
#'
#' Rotate player to direction (x,y,z) specified as a vector
#'
#' @param pos Vector (x,y,z) giving position
#' @param player_id Player ID
#'
#' @return None.
#'
#' @export
#' @importFrom miner setPlayerDirection
#'
#' @seealso [miner::setPlayerDirection()]
#'
#' @examples
#' \dontrun{
#' library(miner)
#' mc_connect()
#' setPlayerDirectionV( c(1, 0, 1) )
#' }

setPlayerDirectionV <-
    function(pos, player_id=NULL)
{
    miner::setPlayerDirection(pos[1], pos[2], pos[3], player_id)
}
