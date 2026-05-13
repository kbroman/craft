#' Draws a line
#'
#' Implements Bresenham's line-drawing algorithm in 3D space. Takes two points,
#' where each point is a vector of 3 coordinates, and constructs a line between
#' the two points.
#'
#' @param p0 vector of the first endpoint
#' @param p1 vector of the second endpoint
#' @param id Minecraft block ID to draw the line with
#' @param style Minecraft block style ID to draw the line with (e.g., color)
#'
#' @return None
#'
#' @author Felix Ling, based on python code in a gist by `theJollySin` that has since been removed
#'
#' @export
#' @importFrom miner setBlock

drawLine <-
function (p0, p1, id = 1, style = 0)
{
    stopifnot(length(p0) == 3)
    stopifnot(length(p1) == 3)

    x <- x0 <- p0[1]
    y <- y0 <- p0[2]
    z <- z0 <- p0[3]

    x1 <- p1[1]
    y1 <- p1[2]
    z1 <- p1[3]

    # Calculate deltas.
    dx = x1 - x0
    dy = y1 - y0
    dz = z1 - z0

    # Calculate the increments.
    sx <- sign (dx)
    sy <- sign (dy)
    sz <- sign (dz)

    # Figure out just the magnitude of change so we can decide which
    # variable to loop/count through
    dx = abs (dx)
    dy = abs (dy)
    dz = abs (dz)

    # The largest difference is the one we'll loop/count through, cuz the other
    # variables may or may not increase by 1 for each increase of the one with
    # the largest difference. If the max is a tie, it doesn't matter which one it
    # loops through.

    # dz is the largest
    if ((dz > dx) && (dz > dy))
    {
       err_x <- dz / 2.0
       err_y <- dz / 2.0

       while (z != z1) {
          setBlock (x, y, z, id, style)

          err_x <- err_x - dx
          if (err_x < 0) {
             x <- x + sx
             err_x <- err_x + dz
          }
          err_y <- err_y - dy
          if (err_y < 0) {
             y <- y + sy
             err_y <- err_y + dz
          }
          z <- z + sz
       }

       # dx largest
    } else if (dx > dy) {

       err_z = dx / 2.0
       err_y = dx / 2.0
       while (x != x1) {
          setBlock (x, y, z, id, style)

          err_y <- err_y - dy
          if (err_y < 0) {
             y <- y + sy
             err_y <- err_y + dx
          }
          err_z <- err_z - dz
          if (err_z < 0) {
             z <- z + sz
             err_z <- err_z + dx
          }
          x <- x + sx

       }

    # dy largest
    } else {
       err_x = dy / 2.0
       err_z = dy / 2.0
       while (y != y1) {
          setBlock (x, y, z, id, style)

          err_x <- err_x - dx
          if (err_x < 0) {
             x <- x + sx
             err_x <- err_x + dy
          }
          err_z <- err_z - dz
          if (err_z < 0) {
             z <- z + sz
             err_z <- err_z +  dy
          }
          y <- y + sy
       }
    }

    setBlock (p1[1], p1[2], p1[3], id, style)
}
