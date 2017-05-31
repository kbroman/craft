# check that dir and top are compatible
check_dirtop <- function(dir, top)
{
    dirs <- list(c("east", "west"),
                 c("north", "south"),
                 c("up", "down"))

    for(d in dirs) {
        if(dir %in% d && top %in% d)
            stop("If dir is ", dir, ", top can't be ", top)
    }
}
