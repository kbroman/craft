# get bitmap for a given character
char2bitmap <- function(char,
                        font=c("4x5", "4x6", "4x8", "6x6", "8x6",
                               "4x12", "6x12", "8x12", "8x16", "16x16"))
{
    font <- match.arg(font)
    if(length(char) > 1) {
        return(lapply(char, char2bitmap, font))
    }
    if(nchar(char) > 1) {
        result <- lapply(strsplit(char, "")[[1]], char2bitmap, font)
        return(do.call("cbind", result))
    }

    data(font_sets, package="miner.extra", envir=parent.frame())

    chars <- font_sets[[font]]$charset
    png <- font_sets[[font]]$png

    chars <- matrix(unlist(strsplit(chars, "")), ncol=length(chars))

    d <- dim(png)
    rows <- seq(1, d[1]+1, by=d[1]/nrow(chars))
    cols <- seq(1, d[2]+1, by=d[2]/ncol(chars))

    # row column coordinates
    wh <- which(chars==char, arr.ind=TRUE)
    if(length(wh) == 0) { # use a space instead
        warning('Character "', char, '" not found')
        wh <- which(chars==" ", arr.ind=TRUE)
    }
    if(length(wh) > 2) wh <- wh[1,]

    rows <- rows[wh[1]]:(rows[wh[1]+1]-1)
    cols <- cols[wh[2]]:(cols[wh[2]+1]-1)

    round(t(1-png[rows,cols]))
}
