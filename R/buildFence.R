#' Build a fence around a square area
#'
#' Build a fence around a square area, adding a gate at a random side, and
#' filling the ground to make a uniform height
#
#'
#' @param length Length and width of fence
#' @param fenceBlock Block ID for fence
#' @param gateBlock Block ID for gate in fence
#' @param player_id Player ID; fence centered at player's current position
#'
#' @return None.
#'
#' @export
#' @importFrom miner getPlayerPos setBlock setBlocks
#'
buildFence <- function (length = 8, fenceBlock = 85, gateBlock=107,
                        player_id = NULL)

{
    stopifnot(length >= 1)

   totalLen <- 4 * length
   heightArray <- vector (length = totalLen)

   pos <- getPlayerPos (player_id, tile = TRUE)
   xStart <- pos[1] - length / 2
   yStart <- pos[2]
   zStart <- pos[3] - length / 2

   i <- 1
   x <- xStart

   # Scope out the heights we'll be building.
   for (z in zStart:(zStart + length)) {
      heightArray[i] <- getHeight (x, z) +1
      setBlock (x, heightArray[i], z, fenceBlock)

      # Checks if there is a height disparity.
      if (z != zStart)
      {
         if (heightArray[i] < heightArray[i - 1])
         {
            setBlocks (x, heightArray[i], z,
                       x, heightArray[i - 1], z, fenceBlock)
         } else if (heightArray[i] > heightArray[i - 1]) {
            setBlocks (x, heightArray[i - 1], z - 1,
                       x, heightArray[i], z - 1, fenceBlock)
         }
      }

      i <- i + 1
   }

   for (x in (xStart+1):(xStart + length)) {
      heightArray[i] <- getHeight (x, z) +1
      setBlock (x, heightArray[i], z, fenceBlock)

      if (x != xStart)
      {
         if (heightArray[i] < heightArray[i - 1])
         {
            setBlocks (x, heightArray[i], z,
                       x, heightArray[i - 1], z, fenceBlock)
         } else if (heightArray[i] > heightArray[i - 1]) {
            setBlocks (x - 1, heightArray[i - 1], z,
                       x - 1, heightArray[i], z, fenceBlock)
         }
      }

      i <- i + 1
   }

   for (z in (zStart + length-1):zStart) {
      heightArray[i] <- getHeight (x, z)+1
      setBlock (x, heightArray[i], z, fenceBlock)

      if (z != zStart + length)
      {
         if (heightArray[i] < heightArray[i - 1])
         {
            setBlocks (x, heightArray[i], z,
                       x, heightArray[i - 1], z, fenceBlock)
         } else if (heightArray[i] > heightArray[i - 1]) {
            setBlocks (x, heightArray[i - 1], z + 1,
                       x, heightArray[i], z + 1, fenceBlock)
         }
      }

      i <- i + 1
   }

    if(length > 1) {
        for (x in (xStart + length-1):(xStart+1)) {
            heightArray[i] <- getHeight (x, z)+1
            setBlock (x, heightArray[i], z, fenceBlock)

            if (x != xStart + length)
            {
                if (heightArray[i] < heightArray[i - 1])
                {
                    setBlocks (x, heightArray[i], z,
                               x, heightArray[i - 1], z, fenceBlock)
                } else if (heightArray[i] > heightArray[i - 1]) {
                    setBlocks (x + 1, heightArray[i - 1], z,
                               x + 1, heightArray[i], z, fenceBlock)
                }
            }
            i <- i + 1
        }
    }


    setBlock(x, heightArray[i-1], z, gateBlock)
}
