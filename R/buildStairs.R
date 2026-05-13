#' Builds stairs
#'
#' Build stairs up or down from player position in the specified direction.
#' For now, width should be odd so that we can have a central staircase and
#' extend to the right and left of it to widen it.
#' @param player_id Player ID
#' @param down Whether to build stairs downward. If false, builds upward
#' @param stopHeight The height at which to stop building.
#' @param stopOnNotAir Whether to stop if about to build where something exists.
#' @param width How wide to build the staircase. MUST BE AN ODD NUMBER
#' @param stairId The block ID for the stairs.
#' @param blockId The block ID for the block underneath each stair. Can set to AIR
#'    if you don't want this block.
#' @param blockStyle The block style for the block underneath each stair.
#' @param buildBlock If false, don't put the blocks under the stairs
#'
#' @details For stairs block IDs, see `miner::find_item("stairs")`; they include
#' 53 for oak, 67 for cobblestone, and 108 for brick. The corresponding blocks for these materials are 5 for oak, 4 for cobblestone, and 45 for brick
#'
#' @importFrom miner getPlayerPos getBlocks
#' @export

buildStairs <- function (player_id=NULL,
                         down = TRUE, stopHeight = 0, stopOnNotAir = TRUE,
                         width = 1, stairId = 109,
                         blockId = 98, blockStyle=0,
                         buildBlock = TRUE)
{
   AIR <- 0

   # Get current player position and heading.
   Pos <- NULL
   while(length(Pos) != 3)
       Pos <- miner::getPlayerPos (player_id, tile = TRUE)
   x <- Pos[1]
   y <- Pos[2]
   z <- Pos[3]

   # width must be odd
   stopifnot(width %% 2 == 1)

   if (down) y <- y - 1

   direction <- getPlayerCompass(player_id, 4)

   # Make sure the input makes sense.
   checkDown <- (stopHeight > y)
   if (!(xor (checkDown, down))) {
      stop ("Input invalid. Current height: ", y, " Stop height: ",
            stopHeight, "Down? ", down)
   }

   # Calculate how many blocks to extend in either direction from the middle.
   # (split this into L and R components that differ if width is even)
   widthExtend <- ceiling ((width - 1) / 2)

   # Set zInc and xInc based on direction.
   # E (+x), W (-x), S (+z), or N (-z)
   zInc <- 0
   xInc <- 0
   if (direction == "north") {
      zInc <- -1
      zWidthInc <- 0
      xWidthInc <- 1
      stairDir <- 2
   } else if (direction == "south") {
      zInc <- 1
      zWidthInc <- 0
      xWidthInc <- 1
      stairDir <- 3
   } else if (direction == "east") {
      xInc <- +1
      xWidthInc <- 0
      zWidthInc <- 1
      stairDir <- 1
   } else {
      xInc <- -1
      xWidthInc <- 0
      zWidthInc <- 1
      stairDir <- 0
   }

   # Set stairDir depending on direction and if going up or down.
   # Add validity check to make sure program will stop!
   if (down) {
      yInc <- -1
      stopHeight <- stopHeight - 1
   } else {
      yInc <- 1
      stopHeight <- stopHeight + 1

      # flip stair direction
      if(stairDir <= 1) stairDir <- 1-stairDir
      else stairDir <- ifelse(stairDir==2, 3, 2)
   }

   # Start one block in that direction (don't build right under the player)
   x <- x + xInc
   z <- z + zInc

   # Build to the specified height.
   while (y != stopHeight) {

      # Check if the stair would destroy anything.
      if (stopOnNotAir) {
         AirCheck <- getBlocks ( x - (widthExtend * xWidthInc), y,
                                 z - (widthExtend * zWidthInc),
                                 x + (widthExtend * xWidthInc), y,
                                 z + (widthExtend * zWidthInc) )

         if (all (AirCheck == AIR)) {
             # Build the stair.
             setBlocksStyle(x - (widthExtend * xWidthInc),
                            y, z - (widthExtend * zWidthInc),
                            x + (widthExtend * xWidthInc),
                            y, z + (widthExtend * zWidthInc),
                            stairId, stairDir)
         }
      }

      if (buildBlock)
      {
         # Check if the block under the stair would destroy anything.
         # ADD CHECK TO SKIP IF UP AND FIRST BLOCK!
         if (stopOnNotAir) {
            AirCheck <- getBlocks ( x - (widthExtend * xWidthInc), y - 1,
                                    z - (widthExtend * zWidthInc),
                                    x + (widthExtend * xWidthInc), y - 1,
                                    z + (widthExtend * zWidthInc) )

            if (all (AirCheck == AIR)) {
                # Build the block beneath the stair.
                setBlocksStyle(x - (widthExtend * xWidthInc),
                               y - 1, z - (widthExtend * zWidthInc),
                               x + (widthExtend * xWidthInc),
                               y - 1, z + (widthExtend * zWidthInc),
                               blockId, blockStyle)
            }
         }
      }

       # Advance to the next iteration.
       x <- x + xInc
       z <- z + zInc
       y <- y + yInc
   }
}
