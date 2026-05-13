#' Builds a door
#'
#' Build a door in the wall (or any non-AIR block) that the player is looking
#' at, opening in that same direction. If desired, also puts a pressure plate in
#' front of and behind the door to automatically open it. Alternatively, can
#' specify the coordinates and direction manually.
#'
#' @param x x-coordinate of where to put the door. If any of `x`, `y`,
#'     or `z` are NULL, will calculate from player's position and
#'     direction they are facing.
#' @param y y-coordinate of where to put the door.
#' @param z z-coordinate of where to put the door.
#' @param direction The compass direction the door should open in.
#' @param doorBlock The material to create the door out of.
#' @param pressurePlate Whether to put pressure plates around the door.
#' @param pressurePlateBlock The material to create the pressure plate out of.
#' @param player_id Player ID
#' @export
#' @importFrom miner getBlock getPlayerPos setBlock setBlocks
#'
#' @author Felix Ling
#'
#' @details For door blocks, see `mc::find_item("door")`; they include
#'     64 for oak and 71 for iron. Pressure place blocks include 70
#'     for stone and 72 for wood.

buildDoor <- function (x = NULL, y = NULL, z = NULL, direction = NULL,
                       doorBlock = 64, pressurePlate = TRUE,
                       pressurePlateBlock = 70,
                       player_id = NULL)
{
   # Find out where the player is looking.
   if (is.null (direction))
      direction <- getPlayerCompass(player_id, 4)

    AIR <- 0

    # Door style constants. Add/OR these together for the style ID.
    DOOR_TOP <- 0x8
    DOOR_BOTTOM <- 0x0

    DOOR_OPEN <- 0x4
    DOOR_CLOSED <- 0x0

    # Note that these do NOT match the stair style ID.
    DOOR_EAST <- 0x0
    DOOR_SOUTH <- 0x1
    DOOR_WEST <- 0x2
    DOOR_NORTH <- 0x3

   # E (+x), W (-x), S (+z), or N (-z)
   xInc <- zInc <- 0
   if (direction == "east") {
      doorDir <- DOOR_EAST
      xInc <- 1
   } else if (direction == "west") {
      doorDir <- DOOR_WEST
      xInc <- -1
   } else if (direction == "south") {
      doorDir <- DOOR_SOUTH
      zInc <- 1
   } else {
      doorDir <- DOOR_NORTH
      zInc <- -1
   }

   if (is.null (x) || is.null (y) || is.null (z)) {
      pos <- getPlayerPos (player_id, tile = TRUE)

      # Move in the direction the player is looking, and up one in case there
      # is carpet
      x <- pos[1] + xInc
      y <- pos[2] + 1
      z <- pos[3] + zInc
      isNotAir <- getBlock (x, y, z, FALSE)

      # Scan in the direction the player is looking until a non-air block is
      # found.
      while (isNotAir == AIR) {
         x <- x + xInc
         z <- z + zInc
         isNotAir <- getBlock (x, y, z, FALSE)
      }

      # Move back down to the floor.
      y <- y - 1
   }

   # Need to clear the space first, or else the top of the door will be covered.
   setBlocks (x, y, z, x, y + 1, z, AIR)

   # Create the door! It's two blocks high and the top and bottom blocks need
   # to be specifically specified.
   setBlock (x, y + 1, z, doorBlock, DOOR_TOP + doorDir)
   setBlock (x, y, z, doorBlock, DOOR_BOTTOM + doorDir)

   # If requested, add a pressure plate in front of and behind the door (thus
   # automatically opening it when you approach it!)
   if (pressurePlate) {
      setBlock (x + xInc, y, z + zInc, pressurePlateBlock)
      setBlock (x - xInc, y, z - zInc, pressurePlateBlock)
   }
}
