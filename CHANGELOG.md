<!-- markdownlint-configure-file {"MD024": { "siblings_only": true } } -->
# Changelog

## Unreleased Version / Daily Erosion Project Development

### Model Changes

- [WEPS] Removed the 8 ms-1 daily wind requirement before calling `erosion`,
  this gate was from previous concerns of runtime.

### New Features

- Allow `-O` or `-o` flags to generate SWEEP input files for the given date
  even if the date does not produce erosion (#5).

### Bug Fixes

- Fixed missing comma typo preventing `-h` (Help) from working on WEPS (#3).
