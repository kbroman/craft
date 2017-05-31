#' Write text in minecraft
#'
#' Write some text in minecraft, using blocks
#'
#' @param text A character string to be written in blocks
#' @param bottomleft Vector of length 3 representing the location for the lower-left corner for the text.
#' @param font Font size to use
#' @param id Block type to use to write the text
#' @param style Block style to use to write the text
#' @param dir Direction the text should run
#' @param top Direction that the top of the text should point
#'
#' @return None
#'
#' @examples
#' \dontrun{
#' library(miner)
#' v <- getPlayerPos()
#' write_text("Hello!", v+c(5,3,0), font="4x8")
#' }
#'
#' @export
write_text <- function(text,
                       bottomleft,
                       font=c("4x5", "4x6", "4x8", "6x6", "8x6",
                              "4x12", "6x12", "8x12", "8x16", "16x16"),
                       id=1,
                       style=0,
                       dir=c("north", "south", "east", "west", "up", "down"),
                       top=c("up", "east", "west", "north", "south", "down"))
{
    font <- match.arg(font)
    dir <- match.arg(dir)
    top <- match.arg(top)

    bmp <- char2bitmap(text, font)
    render_bitmap(bmp, bottomleft, id, style, dir, top)

}
