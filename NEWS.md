## Version 0.1.6 (2019-02-10)

### Minor changes

- Added a couple of additional fonts for `write_text()`. (They were in
  the `font_sets` data set, but weren't included as possibilities in
  the `write_text()` arguments.

### Bug fixes

- Fixed a bug with the `8x6` font (had the bitmap mixed up with the
  6x8 font).

- `craft::write_text()` now works when the package isn't loaded.
