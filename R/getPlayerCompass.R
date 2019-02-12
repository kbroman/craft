#' Get player's rotation  as compass bearing
#'
#' Get the direction that a player is facing as a compass bearing (north, south, east, west, etc.)
#'
#' @md
#'
#' @param player_id Numeric ID for the player
#' @param n_compass_points Number of compass points to use (4, 8, or 16)
#'
#' @return A character string representing the player's direct as a compass bearing
#'
#' @examples
#' \dontrun{
#' library(miner)
#' getPlayerIds()
#' getPlayerRotation(355)
#' getPlayerCompass(355, 8)
#' }
#'
#' @importFrom miner getPlayerRotation
#' @export

getPlayerCompass <- function(player_id=NULL, n_compass_points=c("16","8","4"))
{
    n_compass_points <- as.character(n_compass_points)
    n_compass_points <- match.arg(n_compass_points)

    rot <- miner::getPlayerRotation(player_id)

    rotation_to_compass(rot, n_compass_points)
}


# angle to direction
rotation_to_compass <- function(rotation, n_compass_points=c("16", "8", "4"))
{
    n_compass_points <- as.character(n_compass_points)
    n_compass_points <- match.arg(n_compass_points)

    # ugh I did all these with north == 0 and values 0 to -360
    # but actually south == 0
    dir4 <- c("north", "east", "south", "west")
    angle4 <- c(0, -270, -180, -90)
    dir8 <- c("northeast", "southeast", "southwest", "northwest")
    angle8 <- c(-315, -225, -135, -45)
    dir16 <- c("north-northeast", "east-northeast", "east-southeast", "south-southeast",
               "south-southwest", "west-southwest", "west-northwest", "north-northwest")
    angle16 <- c(-337.5, -292.5, -247.5, -202.5, -157.5, -112.5, -67.5, -22.5)

    if(n_compass_points=="4") {
        dir <- dir4
        angle <- angle4
    } else if(n_compass_points=="8") {
        dir <- c(dir4, dir8)
        angle <- c(angle4, angle8)
    } else {
        dir <- c(dir4, dir8, dir16)
        angle <- c(angle4, angle8, angle16)
    }

    # convert the angles so that south == 0
    angle <- (angle - 180) %% 360

    # add names and add south == 360
    names(angle) <- dir
    angle <- c(angle, "south"=360) # put north in there twice

    rotation <- rotation %% 360

    names(angle)[which.min(abs(angle - rotation))]

}
