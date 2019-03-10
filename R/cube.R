#' Make a cube
#'
#' Make a solid or hollow cube.
#'
#' @param xlen integer. Cube length.
#' @param ylen integer. Cube hight.
#' @param zlen integer. Cube depth.
#' @param blockid integer. Block type.
#' @param styleid integer. Block style.
#' @param fill logical. If TRUE then solid. If FALSE then hollow.
#' @param offset vector of length three. How many units to offset the cube from `pos`.
#' @param playerid integer of length one. Defaults to player id in solo play. Set explicitly in multi play.
#' @param pos vector of length three. Defines the corner of the cube. Defaults to the player's position.
#' @param xlim vector of length two. Defines the lower and upper cuttoff points to truncate the cube.
#' @param ylim vector of length two. Defines the lower and upper cuttoff points to truncate the cube.
#' @param zlim vector of length two. Defines the lower and upper cuttoff points to truncate the cube.
#'
#' @return Builds a cube or cuboid with the corner at the player's position. By default, the cube is hollow, but you can also use `fill=TRUE` to create a solid cube. The players position is determined by [miner::getPlayerPos()]. You can you can reposition the cube with the `offset` command. Use `xlim`, `ylim`, `zlim` to truncate the cube to create a fence or wall. This function will return the cube origin.
#' @export
#' @importFrom miner getPlayerIds getPlayerPos
#'
#' @seealso [sphere()] to create a sphere.
#'
#' @examples
#' \dontrun{
#'
#' # Stone cube 10x10x10
#' cube(pos = c(0, 0, 0))
#'
#' # Erase cube
#' cube(blockid = 0, pos = c(0, 0, 0))
#'
#' # Stone fence
#' cube(ylim = c(2, 3), offset = c(0, -2, 0))
#' }
#'
cube <- function(
  xlen = 10, ylen = 10, zlen = 10,
  blockid = 1,
  styleid = 0,
  fill = FALSE,
  offset = c(0, -1, 0),
  playerid = miner::getPlayerIds(),
  pos, xlim, ylim, zlim
){

    if(missing(pos)){
        if(length(playerid) != 1) stop("playerid must be of length one")
        pos <- miner::getPlayerPos(playerid, tile = TRUE)
    }

    s <- 1 - fill
    xvec <- c(0, rep(s, xlen - 2), 0)
    yvec <- c(0, rep(s, ylen - 2), 0)
    zvec <- c(0, rep(s, zlen - 2), 0)
    prism <- 1 - outer(outer(xvec, yvec), zvec)

    if(missing(xlim)) xlim <- c(1, xlen)
    if(missing(ylim)) ylim <- c(1, ylen)
    if(missing(zlim)) zlim <- c(1, zlen)
    xx <- seq(xlim[1], xlim[2])
    yy <- seq(ylim[1], ylim[2])
    zz <- seq(zlim[1], zlim[2])

    p <- pos + offset
    for(x in xx){
    for(y in yy){
      for(z in zz){
        if(prism[x, y, z] == 1){
          setBlock(x + p[1], y + p[2], z + p[3], blockid, styleid)
        }
      }
    }
    }
    pos
}
