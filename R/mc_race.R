#' Determine who is fastest
#'
#' Hold a race, started in the chat, and see who moves the farthest
#'
#' @param time Length of race (in seconds)
#'
#' @return Return the player_id of whoever moves the farthest
#'
#' @examples
#' \dontrun{
#' library(miner)
#' mc_connect()
#' whoisfastest()
#' }
#'
#' @export
#' @importFrom miner chatPost getPlayerIds getPlayerPos
#' @importFrom stats setNames

mc_race <- function(time=10)
{
    chatPost(paste("Get ready for a ", time, " second race!"))
    Sys.sleep(3)
    chatPost("On your mark...")
    Sys.sleep(2)
    chatPost("Get set...")
    Sys.sleep(2)

    # find players and their positions
    prev_ids <- as.character(getPlayerIds())
    prev_pos <- setNames(lapply(prev_ids, getPlayerPos), prev_ids)

    chatPost("Go!")

    # the race
    Sys.sleep(time)

    chatPost("Finish!")

    # find players and their positions again
    new_ids <- as.character(getPlayerIds())
    new_pos <- setNames(lapply(new_ids, getPlayerPos), new_ids)

    ids <- new_ids[new_ids %in% prev_ids]

    if(length(ids)==0) stop("No valid competitors")
    dist <- sapply(ids, function(id) sqrt(mean((new_pos[[id]] - prev_pos[[id]])^2)))

    winner <- ids[which.max(dist)]
    maxd <- max(dist, na.rm=TRUE)
    chatPost(paste("Player", winner, "won!  RMS distance = ", round(maxd)))

    as.numeric(winner)
}
