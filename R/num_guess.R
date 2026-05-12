#' Guess a number game
#'
#' Play guess-a-number game in the chat
#'
#' @param max_num Maximum number
#' @param delay Delay (in seconds) between calls to the minecraft server
#'
#' @return The correct number
#'
#' @seealso [miner::chatPost()], [miner::getChatPosts()]b
#'
#'
#' @importFrom miner getChatPosts chatPost
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # connect to minecraft
#' miner::mc_connect()
#'
#' # Need to use ctrl-c to stop early
#' num_guess()
#' }


num_guess <-
    function(max_num=100, delay=0.2)
{
    stopifnot(max_num >= 1)

    num <- sample(1:max_num, 1)

    text <- paste0("Hi, I thought a number between 1 and ", max_num,
                   ". Can you guess it? Type numbers in the chat window!")

    chatPost(text)

    while(TRUE){
        ## poll for most recent chat messages
        msg <- getChatPosts()

        ## do nothing if there are no messages since last poll
        if(nrow(msg) > 0) {

            ## iterate through all messages
            for (msgi in seq_len(nrow(msg))) {

                ## check if the message is a number
                numi <- suppressWarnings(as.numeric(as.character(msg[msgi, 'message'])))
                if(is.na(numi)) next

                ## compare numbers
                if (num == numi) {
                    chatPost('Wow, you won!!!')
                    return(num)
                } else {
                    chatPost(sprintf("Nope, it's %s than %s",
                                     ifelse(numi > num, 'smaller', 'greater'),
                                     numi))
                }
            }
        }
        # delay to avoid locking
        Sys.sleep(delay)
    }
}
