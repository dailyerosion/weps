<!-- markdownlint-configure-file {"MD024": { "siblings_only": true } } -->
# Changelog

## Unreleased Version / Daily Erosion Project Development

### Model Changes

- [WEPS] Removed the 8 ms-1 daily wind requirement before calling `erosion`,
  this gate was from previous concerns of runtime.

### New Features

- Allow `-O` or `-o` flags to generate SWEEP input files for the given date
  even if the date does not produce erosion (#5).
- Determine the hourly wind data type by sniffing the file and remove the
  hard coded seven comment line header (#14).

### Bug Fixes

- Allow for a zero clay content soil to not break the model, it is up to the
  user if such a soil is real or not (#21)!
- Fixed missing comma typo preventing `-h` (Help) from working on WEPS (#3).
- Replaced some `stop` instances with explicit `call exit(1)` handling.
