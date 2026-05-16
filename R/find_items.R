#' Find multiple items by name or ID/style
#'
#' Find a set of Minecraft items by name or ID. If querying by ID, the
#' search can also specify item styles.
#'
#' @param name Vector of character string with the names of Minecraft
#'     items (specify either `name` or `id`, not both)
#' @param id Vector of numeric or character strings with the ID of a
#'     Minecraft item (specify either `name` or `id`, not both)
#' @param style Vector of numeric or character string with the style
#'     of a Minecraft item (use this argument only if querying by `id`
#'     is provided); should have length 1 or the same length as `id`.
#'
#' @return Data frame with a row for each item found in [`mc_items`],
#'     provided the matches are unique.
#'
#' @details If `name` is provided, we first look to see whether there
#' is an exact match to the `name` column in [`mc_items`]. If there
#' is, we return that row. If not, we don't include the results.
#'
#' If instead `id` is provided, we return the row with that id
#' and `style==0` (or whatever style was provided).
#'
#' @seealso [miner::find_item()], [miner::mc_items]
#'
#' @examples
#' flower_names <- c("Chorus Flower", "Peony", "Rose Bush",
#'     "Lilac", "Sunflower", "Pink Tulip", "White Tulip",
#'     "Orange Tulip", "Red Tulip", "Oxeye Daisy", "Allium",
#'     "Dandelion", "Poppy", "Blue Orchid", "Peony")
#' flowers <- find_items(flower_names)
#'
#' @export
#' @importFrom miner find_item

find_items <-
    function(name=NULL, id=NULL, style=0)
{
    if(!is.null(name) && !is.null(id)) {
        stop("Provide either name or id but not both")
    }

    if(!is.null(name)) {
        suppressMessages(result <- lapply(name, find_item))
    } else {
        if(length(style)==1) style <- rep(style, length(id))
        stopifnot(length(style) == length(id))

        result <- lapply(seq_along(id), function(i)
            suppressMessages(find_item(id=id[i], style=style[i])))
    }

    # only take the unique results
    nr <- sapply(result, function(x) ifelse(is.null(x), 0, nrow(x)))
    result <- result[nr ==1]

    if(length(result)==0) {
        warning("No items found")
        return(NULL)
    }

    do.call("rbind", result)
}
