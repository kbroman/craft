#' Scatterplot within Minecraft
#'
#' Make a scatterplot within Minecraft
#'
#' @param lowerleft Vector of length 3, specifying the position of the lower-left corner of the plot.
#' @param x Vector of x values
#' @param y Vector of y values
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @param group Vector of groups for coloring the points
#' @param width Width of plot in blocks
#' @param height Height of plot in blocks
#' @param dir Direction the plot will go
#' @param top Direction for the top of the plot
#' @param block_colors A list of block IDs and styles to denote the colors to be used; needs to contain
#' \code{"gray"}, \code{"white"}, \code{"black"}, and \code{"colors"}.
#'
#' @return Returns the input \code{lowerleft}.
#'
#' @seealso \code{\link{mc_clearplot}}
#'
#' @examples
#' \dontrun{
#' v <- mc_plot(getPlayerPos()+c(0, 5, 5),
#'              x=iris$Sepal.Length, y=iris$Sepal.Width,
#'              group=iris$Species,
#'              xlab="Sepal.Length", ylab="Sepal.Width")
#' }
#'
#' @importFrom miner setBlock
#' @export
# make a scatterplot
mc_plot <-
    function(lowerleft=miner::getPlayerPos()+c(0,5,5),
             x, y, xlab="x", ylab="y", group=NULL,
             width=120, height=120,
             dir=c("east", "west", "north", "south", "up", "down"),
             top=c("up", "north", "south", "east", "west", "down"),
             block_colors=list(gray=c(35,8), white=c(35,0), black=c(35,15),
                               colors=cbind(35, c(9,14,5))))
{
    gray <- block_colors$gray
    white <- block_colors$white
    black <- block_colors$black
    colors <- block_colors$colors
    if(is.null(gray)) gray <- c(35, 8)
    if(is.null(black)) black <- c(35, 15)
    if(is.null(white)) white <- c(35, 0)
    if(is.null(colors)) colors <- cbind(35, c(9,14,5))

    stopifnot(length(x) == length(y))
    if(is.null(group)) group <- rep(1, length(x))
    stopifnot(length(group) == length(x))
    stopifnot(length(unique(group)) <= nrow(colors))

    # check directions are compatible
    dir <- match.arg(dir)
    top <- match.arg(top)
    check_dirtop(dir, top)

    # convert group to numeric
    if(is.factor(group)) {
        group <- as.numeric(group)
    } else {
        group <- match(group, unique(group))
    }

    # margins
    lowmarg <- 16
    lefmarg <- 16

    xlim <- range(x) + diff(range(x)) * c(-0.05, 0.05)
    ylim <- range(y) + diff(range(y)) * c(-0.05, 0.05)
    xscale <- intscale(xlim, c(1+lefmarg, width))
    yscale <- intscale(ylim, c(1+lowmarg, height))

    xtick <- pretty(xlim, 5)
    ytick <- pretty(ylim, 5)
    xtick <- xtick[xtick >= xlim[1] & xtick <= xlim[2]]
    ytick <- ytick[ytick >= ylim[1] & ytick <= ylim[2]]
    xtick_sc <- xscale(xtick)
    ytick_sc <- yscale(ytick)

    xtick_ch <- tick2char(xtick)
    ytick_ch <- tick2char(ytick)

    xval <- (lefmarg+1):width
    yval <- (lowmarg+1):height

    ew <- c("east", "west")
    ns <- c("north", "south")
    ud <- c("up", "down")

    if(dir %in% c("west", "north", "down")) dir_mult <- -1
    else dir_mult <- +1
    if(top %in% c("west", "north", "down")) top_mult <- -1
    else top_mult <- +1


    # gray rectangle
    X <- lowerleft[1]
    Y <- lowerleft[2]
    Z <- lowerleft[3]
    for(i in xval) {
        for(j in yval) {
            if(dir %in% ew) X <- lowerleft[1] + dir_mult * i
            else if(dir %in% ns) Z <- lowerleft[3] + dir_mult * i
            else Y <- lowerleft[2] + dir_mult * i

            if(top %in% ew) X <- lowerleft[1] + top_mult * j
            else if(top %in% ns) Z <- lowerleft[3] + top_mult * j
            else Y <- lowerleft[2] + top_mult * j

            setBlock(X, Y, Z, gray[1], gray[2])
        }
    }

    # grid lines
    X <- lowerleft[1]
    Y <- lowerleft[2]
    Z <- lowerleft[3]
    for(i in xtick_sc) {
        for(j in yval) {
            if(dir %in% ew) X <- lowerleft[1] + dir_mult * i
            else if(dir %in% ns) Z <- lowerleft[3] + dir_mult * i
            else Y <- lowerleft[2] + dir_mult * i

            if(top %in% ew) X <- lowerleft[1] + top_mult * j
            else if(top %in% ns) Z <- lowerleft[3] + top_mult * j
            else Y <- lowerleft[2] + top_mult * j

            setBlock(X, Y, Z, white[1], white[2])
        }
    }
    X <- lowerleft[1]
    Y <- lowerleft[2]
    Z <- lowerleft[3]
    for(i in xval) {
        for(j in ytick_sc) {
            if(dir %in% ew) X <- lowerleft[1] + dir_mult * i
            else if(dir %in% ns) Z <- lowerleft[3] + dir_mult * i
            else Y <- lowerleft[2] + dir_mult * i

            if(top %in% ew) X <- lowerleft[1] + top_mult * j
            else if(top %in% ns) Z <- lowerleft[3] + top_mult * j
            else Y <- lowerleft[2] + top_mult * j

            setBlock(X, Y, Z, white[1], white[2])
        }
    }

    # points
    X <- lowerleft[1]
    Y <- lowerleft[2]
    Z <- lowerleft[3]
    for(i in seq_along(x)) {
            if(dir %in% ew) X <- lowerleft[1] + dir_mult * xscale(x[i])
            else if(dir %in% ns) Z <- lowerleft[3] + dir_mult * xscale(x[i])
            else Y <- lowerleft[2] + dir_mult * xscale(y[i])

            if(top %in% ew) X <- lowerleft[1] + top_mult * yscale(y[i])
            else if(top %in% ns) Z <- lowerleft[3] + top_mult * yscale(y[i])
            else Y <- lowerleft[2] + top_mult * yscale(y[i])

            setBlock(X, Y, Z, colors[group[i],1], colors[group[i], 2])
    }

    # ticks
    X <- lowerleft[1]
    Y <- lowerleft[2]
    Z <- lowerleft[3]
    for(i in xtick_sc) {
        for(j in lowmarg - (0:1)) {
            if(dir %in% ew) X <- lowerleft[1] + dir_mult * i
            else if(dir %in% ns) Z <- lowerleft[3] + dir_mult * i
            else Y <- lowerleft[2] + dir_mult * i

            if(top %in% ew) X <- lowerleft[1] + top_mult * j
            else if(top %in% ns) Z <- lowerleft[3] + top_mult * j
            else Y <- lowerleft[2] + top_mult * j

            setBlock(X, Y, Z, black[1], black[2])
        }
    }
    X <- lowerleft[1]
    Y <- lowerleft[2]
    Z <- lowerleft[3]
    for(i in lefmarg - (0:1)) {
        for(j in ytick_sc) {
            if(dir %in% ew) X <- lowerleft[1] + dir_mult * i
            else if(dir %in% ns) Z <- lowerleft[3] + dir_mult * i
            else Y <- lowerleft[2] + dir_mult * i

            if(top %in% ew) X <- lowerleft[1] + top_mult * j
            else if(top %in% ns) Z <- lowerleft[3] + top_mult * j
            else Y <- lowerleft[2] + top_mult * j

            setBlock(X, Y, Z, black[1], black[2])
        }
    }

    # x-axis scale
    X <- lowerleft[1]
    Y <- lowerleft[2]
    Z <- lowerleft[3]
    for(i in seq_along(xtick_sc)) {
        pos <- (xtick_sc[i] - nchar(xtick_ch[i])*2 + 1)
        if(dir %in% ew) X <- lowerleft[1] + dir_mult * pos
        else if(dir %in% ns) Z <- lowerleft[3] + dir_mult * pos
        else Y <- lowerleft[2] + dir_mult * pos

        if(top %in% ew) X <- lowerleft[1] + lowmarg - top_mult * 9
        else if(top %in% ns) Z <- lowerleft[3] + lowmarg - top_mult * 9
        else Y <- lowerleft[2] + lowmarg - top_mult * 9

        write_text(xtick_ch[i], c(X,Y,Z), font="4x6", dir=dir, top=top,
                   id=black[1], style=black[2])
    }

    # y-axis scale
    X <- lowerleft[1]
    Y <- lowerleft[2]
    Z <- lowerleft[3]
    for(i in seq_along(ytick_sc)) {
        pos <- (lefmarg - nchar(ytick_ch[i])*3 - 4)
        if(dir %in% ew) X <- lowerleft[1] + dir_mult * pos
        else if(dir %in% ns) Z <- lowerleft[3] + dir_mult * pos
        else Y <- lowerleft[2] + dir_mult * pos

        pos <- ytick_sc[i] - 4
        if(top %in% ew) X <- lowerleft[1] + top_mult*pos
        else if(top %in% ns) Z <- lowerleft[3] + top_mult*pos
        else Y <- lowerleft[2] + top_mult*pos

        write_text(ytick_ch[i], c(X,Y,Z), font="4x6", dir=dir, top=top,
                   id=black[1], style=black[2])
    }


    # x-axis label
    X <- lowerleft[1]
    Y <- lowerleft[2]
    Z <- lowerleft[3]
    xpos <- mean(xval) - nchar(xlab)*2
    if(dir %in% ew) X <- lowerleft[1] + dir_mult * xpos
    else if(dir %in% ns) Z <- lowerleft[3] + dir_mult * xpos
    else Y <- lowerleft[2] + dir_mult * xpos

    ypos <- (lowmarg - 16)
    if(top %in% ew) X <- lowerleft[1] + top_mult * ypos
    else if(top %in% ns) Z <- lowerleft[3] + top_mult * ypos
    else Y <- lowerleft[2] + top_mult * ypos

    write_text(xlab, c(X,Y,Z), font="4x6", dir=dir, top=top,
               id=black[1], style=black[2])

    # y-axis label
    X <- lowerleft[1]
    Y <- lowerleft[2]
    Z <- lowerleft[3]
    xpos <- lefmarg - max(nchar(ytick_ch))*6 - 1
    if(dir %in% ew) X <- lowerleft[1] + dir_mult * xpos
    else if(dir %in% ns) Z <- lowerleft[3] + dir_mult * xpos
    else Y <- lowerleft[2] + dir_mult * xpos

    ypos <- mean(yval) - nchar(ylab)*2
    if(top %in% ew) X <- lowerleft[1] + top_mult * ypos
    else if(top %in% ns) Z <- lowerleft[3] + top_mult * ypos
    else Y <- lowerleft[2] + top_mult * ypos

    if(nchar(ylab) == 1) # if one character label, don't rotate
        write_text(ylab, c(X,Y,Z), font="4x6", dir=dir, top=top,
                   id=black[1], style=black[2])
    else
        write_text(ylab, c(X,Y,Z), font="4x6", dir=top, top=revdir(dir),
                   id=black[1], style=black[2])


    lowerleft
}


#' Clear scatterplot
#'
#' Remove a scatterplot created with \code{\link{mc_plot}} by replacing the blocks with air.
#'
#' @param lowerleft Vector of length 3, specifying the position of the lower-left corner of the plot.
#' @param x Vector of x values
#' @param y Vector of y values
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @param width Width of plot in blocks
#' @param height Height of plot in blocks
#' @param dir Direction the plot will go
#' @param top Direction for the top of the plot
#'
#' @return None.
#'
#' @seealso \code{\link{mc_clearplot}}
#'
#' @examples
#' \dontrun{
#' v <- mc_plot(x=iris$Sepal.Length, y=iris$Sepal.Width, group=iris$Species,
#'              xlab="Sepal.Length", ylab="Sepal.Width")
#' Sys.sleep(10)
#' mc_clearplot(v, x=iris$Sepal.Length, y=iris$Sepal.Width,
#'              xlab="Sepal.Length", ylab="Sepal.Width")
#' }
#'
#' @importFrom miner setBlock
#'
#'
#' @export
mc_clearplot <-
    function(lowerleft,
             x, y, xlab="x", ylab="y",
             width=120, height=120,
             dir=c("east", "west", "north", "south", "up", "down"),
             top=c("up", "north", "south", "east", "west", "down"))

{
    # check directions are compatible
    dir <- match.arg(dir)
    top <- match.arg(top)
    check_dirtop(dir, top)

    mc_plot(lowerleft=lowerleft, x=x, y=y, xlab=xlab, ylab=ylab,
            group=NULL, height=height, width=width,
            block_colors=list(gray=c(0,0), white=c(0,0), black=c(0,0),
                              colors=cbind(0, 0)),
            dir=dir, top=top)
}

# create a function that scales numbers in in_lim to integers in out_lim
#
# for example:
#   f <- intscale(c(0,1), c(1, 200))
#   f(0.3)  # returns 61
intscale <- function(in_lim, out_lim)
{
    return(  function(x) round((x-in_lim[1])*diff(out_lim)/diff(in_lim) + out_lim[1])   )
}

# tick values to character strings (same number past decimal)
tick2char <- function(ticks)
{
    ticks <- as.character(ticks)

    if(any(grepl("\\.", ticks))) {
        ndig <- sapply(strsplit(ticks, "\\."),
                       function(a) ifelse(length(a)==1, 0, nchar(a[2])))
        max_dig <- max(ndig)
        if(any(ndig < max_dig)) {
            dig2add <- max_dig - ndig
            for(i in seq_along(dig2add)) {
                if(!grepl("\\.", ticks[i])) ticks[i] <- paste0(ticks[i], ".")
                if(dig2add[i] > 0) ticks[i] <- paste0(ticks[i], paste(rep("0", dig2add[i]), collapse=""))
            }
        }
    }

    ticks
}

# reverse direction
revdir <- function(dir)
{
    if(dir=="east") return("west")
    if(dir=="west") return("east")
    if(dir=="north") return("south")
    if(dir=="south") return("north")
    if(dir=="up") return("east")
    if(dir=="down") return("up")
}
