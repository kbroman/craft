## Version 0.3.5 (2026-05-16)

### New features

- Added function `find_items()` for finding mulitiple items (with
  exact matches) from `miner::mc_items`.

- Added `block_style` argument to `ice_towers()`.


## Version 0.3.4 (2026-05-16)

### New features

- Added `getBlockV()` and `getBlocksV()` which are like `getBlock()` and
  `getBlocks()` but takes vector positions.


## Version 0.3.3 (2026-05-14)

### New features

- Added a vignette, available online at <https://kbroman.org/craft/craft.html>

- Added function `mc_race()`. Identify who moved farthest in a 10
  second race, based on code in Chapter 11 of
  [ebook](https://kbroman.org/miner_book/)

### Minor revisions

- In `mc_maze()`, moved the maze a bit, so that the entrance is right
  by the player. Also rotate the player toward the door and add a
  couple of stairs.


## Version 0.3.2 (2026-05-13)

### New features

- Added dataset `Rlogo` and function `buildRlogo()`, based on [Chapter
  9](https://kbroman.org/miner_book/rendering-the-r-logo-in-minecraft.html).
  (The script `inst/scripts/r_logo.R` was used to create the dataset.)

- Added `clearSpace()` to clear the space around a player (filling it
  with air).

- Added `whereami()` which is just like `miner::getPlayerPos()` but
  with `tile=TRUE` being the default.

- Added `setBlocksStyle()` which is like `setBlocks()` but also takes
  a `style` argument (by repeatedly calling `setBlock()`.

- Added `setBlocksMix()` which is like `setBlocks()` but filling in
  with random selection of block types.

- Added function `drawLine()` for drawing a line.

- Added functions `buildStairs()`, `buildDoor()`, `buildBuilding()`,
  and `buildFence()`.

- Added functions `setBlockV()`, `setBlocksV()`, `setBlocksStyleV()`,
  `setBlocksMixV()`, `setPlayerPosV()`, and `setPlayerDirectionV()`
  which are like the versions without `V` in the name, but taking a
  vector as a position rather than three separate values.


## Verison 0.2.1 (2026-05-12)

### New features

- Added function `ice_towers()` from [Chapter 6](https://kbroman.org/miner_book/planting-a-garden.html)
  of the [R Programming with Minecraft](https://kbroman.org/miner_book/) book.

- Added function `num_guess()` based on code in [Chapter 7](https://kbroman.org/miner_book/number-guess-chat-bot-in-minecraft.html).


## Version 0.1.9 (2020-12-14)

### Minor changes

- The craft package was moved from ropenscilabs to kbroman,
  <https://github.com/kbroman/craft>


## Version 0.1.8 (2019-03-10)

### Minor changes

- Convert documentation to markdown


## Version 0.1.7 (2019-02-11)

### New features

- Added `getPlayerCompass()` for getting a player's rotation as a
  compass heading (e.g., "east-northeast")

### Minor changes

- Added a couple of additional fonts for `write_text()`. (They were in
  the `font_sets` data set, but weren't included as possibilities in
  the `write_text()` arguments.

- More informative warning message from write_text() if characters are
  not present in the selected font.

### Bug fixes

- Fixed a bug with the `8x6` font (had the bitmap mixed up with the
  6x8 font).

- `craft::write_text()` now works when the package isn't loaded.
