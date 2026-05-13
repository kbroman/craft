#' Determine player's compass direction
#'
#' Figure out the largest value of a player's direction and translate
#' to a compass direction.
#'
#' @param player_id Player ID
#'
#' @param method get player direction either from
#'     [miner::getPlayerRotation()] or [miner::getPlayerDirection()].
#'
#' @author Felix Ling
#'
#' @return Character string indication player direction
#'
#' @export
#' @importFrom miner getPlayerDirection getPlayerRotation
#'
getPlayerCompassDir <-
function (player_id = NULL, method=c("rotation", "direction"))
{
    method <- match.arg(method)

    if(method=="rotation") {
        rot <- NA
        while(is.na(rot)) suppressWarnings(rot <- getPlayerRotation(player_id))

        # make it a non-negative number
        while(rot < 0) rot <- rot + 360
        rot <- rot %% 360

        # cut at 45 degrees
        interval <- as.numeric(cut(rot, c(0, 45, 135, 225, 315, 360)))
        return(c("south", "west", "north", "east", "south")[interval])
    }

    vec <- NULL
    while(length(vec) < 3) vec <- miner::getPlayerDirection(player_id)

    # E (+x), W (-x), S (+z), or N (-z)
    # See if magnitude of x is bigger than magnitude of z. Ignore y.
    if (abs(vec[1]) > abs(vec[3])) {
        if (vec[1] > 0)
            return ("east")
        else
            return ("west")
    } else {
        if (vec[3] > 0)
            return ("south")
        else
            return ("north")
    }
}
