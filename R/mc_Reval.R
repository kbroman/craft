#' Evaluate R within Minecraft
#'
#' Look for chat messages that start with `"R "` and then evaluate the rest as an R expression.
#'
#' @param player_id Player's entity ID
#' @param delay Delay (in seconds) between checks of the chat messages
#'
#' @return None.
#'
#' @details The function looks at chat messages in Minecraft. For messages
#' that begin `"R "`, the rest of the message is treated as an R expression
#' and is evaluated.
#'
#' The default is to evaluate any such R code. If `player_id` is
#' specified, only messages from that player are considered.
#'
#' Use **extreme** caution with this; if you don't specify
#' `player_id` correctly, you could be letting others execute
#' code on your computer and so giving them the opportunity to do
#' bad things.
#'
#' @importFrom miner getChatPosts chatPost
#' @importFrom utils capture.output
#' @export
#'
#' @examples
#' \dontrun{
#' # runs in an infinite loop; use ctrl-c to stop
#' mc_Reval(212)}
mc_Reval <- function(player_id=NULL, delay=1)
{
    # toss what's there
    junk <- miner::getChatPosts()

    while(TRUE) {

        posts <- miner::getChatPosts()
        if(nrow(posts)==0) next

        posts <- posts[grepl("^R\\s+", posts$message),,drop=FALSE]
        if(nrow(posts)==0) next

        if(!is.null(player_id)) {
            posts <- posts[posts$player==player_id,,drop=FALSE]
            if(nrow(posts)==0) next
        }

        code <- sub("^R\\s+", "", posts$message)
        for(line in code) {
            output <- eval(parse(text=line), envir=.GlobalEnv)

            if(!is.null(output)) {
                printed_output <- paste(utils::capture.output( output ), collapse="\n")
                print(printed_output)
                if(length(printed_output) > 0) {
                    for(line in printed_output) {
                        miner::chatPost(line)
                        Sys.sleep(delay)
                    }
                }
            }
        }

        Sys.sleep(delay)
    }
}
