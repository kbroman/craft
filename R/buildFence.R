#' Build a fence around a square area
#'
#' Build a fence around a square area, adding a gate in a random side, and
#' filling the ground underneath to make a uniform height
#'
#' @param length Length and width of fence (must be >= 3)
#' @param fenceBlock Block ID for fence
#' @param gateBlock Block ID for gate in fence
#' @param foundationBlock Block ID for foundation (to get everything to a uniform height)
#' @param player_id Player ID; fence centered at player's current position
#'
#' @return None.
#'
#' @note Having trouble getting the fence to connect all the way around".
#'
#' @export
#' @importFrom miner getPlayerPos setBlock setBlocks getHeight
#' @importFrom stats median
#'
buildFence <- function (length = 8, fenceBlock = 85, gateBlock=107,
                        foundationBlock=1,
                        player_id = NULL)

{
    length <- round(length)
    stopifnot(length >= 3)
    odd <- as.logical(length %% 2)

    pos <- miner::getPlayerPos (player_id, tile = TRUE)
    xpos <- pos[1]
    zpos <- pos[3]

    if(odd) {
        left <- -(length-1)/2
        right <- (length-1)/2
    } else {
        left <- -(length/2-1)
        right <- length/2
    }

    corners = data.frame(x=c(xpos+right, xpos+left, xpos+left, xpos+right),
                         z=c(zpos+right, zpos+right, zpos+left, zpos+left))

    sides <- heights <- vector("list", 4)
    sides[[1]] <- data.frame(x=seq(corners$x[1], corners$x[2], by=-1),
                             z=rep(corners$z[1], length))
    sides[[2]] <- data.frame(x=rep(corners$x[2], length),
                             z=seq(corners$z[2], corners$z[3], by=-1))
    sides[[3]] <- data.frame(x=seq(corners$x[3], corners$x[4], by=1),
                             z=rep(corners$z[3], length))
    sides[[4]] <- data.frame(x=rep(corners$x[4], length),
                             z=seq(corners$z[4], corners$z[1], by=1))


    for(i in 1:4) {
        heights[[i]] <- rep(NA, length-1)
        for(p in 1:nrow(sides[[i]])) {
            heights[[i]][p] <- miner::getHeight(sides[[i]]$x[p], sides[[i]]$z[p])
        }
    }


    # fill in holes
    min_height <- min(unlist(heights))
    max_height <- max(unlist(heights))

    if(min_height < max_height) {

        for(side in seq_along(sides)) {
            for(p in seq_along(heights[[side]])) {
                if(heights[[side]][p] < max_height) {
                    setBlocks(sides[[side]]$x[p], heights[[side]][p]+1, sides[[side]]$z[p],
                              sides[[side]]$x[p], max_height,           sides[[side]]$z[p], foundationBlock)
                }
            }
        }
    }


    # add fence
    for(side in seq_along(sides)) {
        for(x in min(sides[[side]]$x):max(sides[[side]]$x)) {
            for(z in min(sides[[side]]$z):max(sides[[side]]$z)) {
                setBlock(x, max_height+1, z, fenceBlock)
            }
        }
    }

    # add a random gate
    g <- sample(seq_along(sides), 1)
    gateStyle <- c(0,1,0,1)[g]
    setBlock(median(sides[[g]]$x), max_height+1, median(sides[[g]]$z), gateBlock, gateStyle)

}
