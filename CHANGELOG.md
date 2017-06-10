Change Log
==========

All notable changes to this project will be documented in this file, which
follows the conventions of [keepachangelog.com](http://keepachangelog.com/).
This project adheres to [Semantic Versioning](http://semver.org/).

## [Unreleased]

...

## [0.3.0] - 2017-06-09

### Changed
- `:context-gen` option shortened to `:context`.
- Exceptions thrown during an operation are now caught and used as the operation
  result.
- World worker thread poll time shortened to 100 ms.

### Fixed
- World states visited in a linear scan are properly counted.

## [0.2.0] - 2017-05-25

This release includes substantial optimizations to searching for valid
worldlines during concurrency tests. Rather than naively searching whole
histories in a rigid order, worlds are evolved a step at a time, with cheaper
worlds (those with fewer possible futures) searched first.

### Added
- New world record and functions in `test.carly.world` namespace.

### Changed
- Moved operation protocol and functions to new `test.carly.op` namespace.
- Many optimizations to the concurrency test world searching.

## [0.1.0] - 2017-05-21

Initial project release.

[Unreleased]: https://github.com/greglook/test.carly/compare/0.3.0...HEAD
[0.3.0]: https://github.com/greglook/test.carly/compare/0.2.0...0.3.0
[0.2.0]: https://github.com/greglook/test.carly/compare/0.1.0...0.2.0
[0.1.0]: https://github.com/greglook/test.carly/tag/0.1.0
