#' Listen for "Who am I"
#'
#' Listen for "Who am I" on minecraft chat, and respond with the player's entity ID.
#'
#' @param delay Delay (in seconds) between checks of the chat messages
#' @param max_time Maximum amount of time (in seconds) before quitting.
#'
#' @return A vector of player IDs
#'
#' @importFrom miner getChatPosts chatPost
#' @export
#'
#' @details Uses [base::grepl()], ignoring case, to look for
#'     "who am I" in the chat messages, and prints the player ID of
#'     any player who posts a chat message containing that phrase.
#'
#' @examples
#' \donttest{
#' #' will run for 15 seconds
#' mc_whoami(max_time=15)}
mc_whoami <- function(delay=1, max_time = Inf)
{
    start_time <- proc.time()[3]

    result <- NULL
    while(TRUE) {

        posts <- miner::getChatPosts()
        if(nrow(posts)==0) next

        whoami <- grepl("who am i", posts$message, ignore.case=TRUE)

        if(any(whoami)) {
            players <- posts$player[whoami]

            result <- c(result, players)

            for(player in players) {
                miner::chatPost(paste0("You are ", player, "."))
                message("You are ", player, ".")
                Sys.sleep(delay)
            }
        }

        if(proc.time()[3] - start_time > max_time) break
        Sys.sleep(delay)
    }

    result
}
