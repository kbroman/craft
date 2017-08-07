#' Make a sphere
#'
#' Make a solid or hollow sphere with the player at the center.
#'
#' @param radius integer. Specifies the radius of the sphere.
#' @param blockid integer. Block type to be used.
#' @param styleid integer. Block style to be used.
#' @param fill logical. Should the sphere be solid or hollow. Defaults to hollow.
#' @param offset vector of length three. Defines where to place the sphere with respect to \code{pos}.
#' @param playerid integer of length one. Defaults to player id in solo play. Set explicitly in multi play.
#' @param pos vector of length three. Defines the center of the sphere. Defaults to the player id position.
#' @param xlim vector of length two. Defines the lower and upper cuttoff points to truncate the sphere.
#' @param ylim vector of length two. Defines the lower and upper cuttoff points to truncate the sphere.
#' @param zlim vector of length two. Defines the lower and upper cuttoff points to truncate the sphere.
#'
#' @return Builds a sphere (or a truncated sphere) around the players current position. By default, the sphere is hollow, but you can also use \code{fill=TRUE} to create a solid sphere. The players position is determined by \code{getPlayerPos}. You can you can reposition the sphere with the \code{offset} command. Use \code{xlim}, \code{ylim}, \code{zlim} to truncate the sphere. This function will return the sphere's origin.
#' @export
#'
#' @seealso \code{\link{cube}} to create a cube.
#'
#' @examples
#' \dontrun{
#' 
#' # Hollow glass sphere of size 15
#' sphere(pos = c(0, 0, 0))
#' 
#' # Erase sphere
#' sphere(blockid = 0, pos = c(0, 0, 0))
#' 
#' # Glass dome
#' sphere(ylim = c(0, 15))
#' }
#' 
sphere <- function(
    radius = 15,
    blockid = 20,
    styleid = 0,
    fill = FALSE,
    offset = c(0, 0, 0),
    playerid = getPlayerIds(),
    pos, xlim, ylim, zlim
){

    if(missing(pos)){
        if(length(playerid) != 1) stop("playerid must be of length one")
        pos <- getPlayerPos(playerid, tile = TRUE)
    }

    # Create a stack of rings
    rings <- array(0, c(x = 2 * radius + 1, y = radius + 1, z = 2 * radius + 1))
    for(y in 0:radius){
      for(theta in seq(0, 2 * pi, len = 10 * radius)){
        xind <- sqrt(radius ^ 2 - y ^ 2) * cos(theta) + radius
        zind <- sqrt(radius ^ 2 - y ^ 2) * sin(theta) + radius
        rings[round(xind) + 1, y + 1, round(zind) + 1] <- 1
      }
    }

    # Create a solid dome
    fillrow <- function(x){
      ind <- which(x == 1)
      if(length(ind) == 0) ind <- 0
      rng <- range(ind)
      idx <- seq_along(x)
      fill <- idx >= min(rng) & idx <= max(rng)
      fill * 1
    }
    dome <- aperm(apply(rings, 1:2, fillrow), c(2, 3, 1))

    # Hollow out the dome
    if(!fill){
      for(y in 1:radius){
        disk <- (dome[, y, ] - dome[, y + 1, ]) == 1
        ring <- rings[, y, ] == 1
        dome[, y, ] <- (disk | ring) * 1
      }
    }

    # Create a sphere
    yind <- abs(seq(-radius, radius)) + 1
    sphere <- dome[, yind, ,drop = FALSE]

    # Define plot dimensions
    if(missing(xlim)) xlim <- c(-radius, radius)
    if(missing(ylim)) ylim <- c(-radius, radius)
    if(missing(zlim)) zlim <- c(-radius, radius)
    xx <- seq(xlim[1], xlim[2]) + radius + 1
    yy <- seq(ylim[1], ylim[2]) + radius + 1
    zz <- seq(zlim[1], zlim[2]) + radius + 1

    # Place blocks to create the sphere
    p <- pos + offset - c(radius + 1, radius + 1, radius + 1)
    for(x in xx){
      for(y in yy){
        for(z in zz){
          if(sphere[x, y, z] == 1){
            setBlock(x + p[1], y + p[2], z + p[3], blockid, styleid)
          }
        }
      }
    }
    pos
}
