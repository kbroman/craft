#' Build a building
#'
#' Build a building with specified dimensions, with a foundation, walls, floorboards, carpet, and a door
#'
#' @param length Length of the building
#' @param width Width of the building
#' @param height Height of the building
#' @param foundation ID for foundation blocks (1 = stone)
#' @param wall ID for wall blocks (45 = brick)
#' @param floorBoards ID for floor boards (125 = wood planks)
#' @param carpet ID for carpet blocks (171 = carpet)
#' @param carpet_style ID for carpet style (11 = blue)
#' @param gap ID for gap blocks (0 = air)
#' @param player_id Player ID
#'
#' @return None.
#'
#' @export
#' @importFrom miner getPlayerPos setBlocks
#'
#' @author Felix Ling

buildBuilding <- function (length = 8, width = 6, height = 5,
                           foundation = 1,
                           wall = 45,
                           floorBoards = 125,
                           carpet = 171,
                           carpet_style = 11,
                           gap = 0,
                           player_id=NULL)
{

   # Get the player position
   pos <- getPlayerPos (player_id, tile = TRUE)
   x <- pos[1]
   y <- pos[2]
   z <- pos[3]

   x = x - (width / 2)
   z = z - (width / 2)

   # Build the foundation.
   setBlocks (x, y - 2, z,
              x + width, y - 2, z + length, foundation)

   # Build the outer shell of the house
   setBlocks (x, y, z,
              x + width, y + height, z + length, wall)

   # Carve the insides out with AIR
   setBlocks (x + 1, y, z + 1,
              x + width - 1, y + height - 1, z + length - 1, gap)

   # Build the floor and carpet it.
   setBlocks (x + 1, y - 1, z + 1,
              x + width - 1, y - 1, z + length - 1, floorBoards)
   setBlocksStyle (x + 1, y, z + 1,
                   x + width - 1, y, z + length - 1, carpet, carpet_style)

   # Build the door.
   setBlocks (x + width / 2, y, z,
              x + width / 2, y + 1, z, gap)

   buildDoor (x + width / 2, y, z, player_id=player_id)
}
