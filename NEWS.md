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
