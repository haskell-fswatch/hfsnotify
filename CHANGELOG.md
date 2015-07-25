Changes
=======

Version 0.2
-----------

Use filepath instead of deprecated system-filepath

Version 0.1.0.3
---------------

* Fix the tests

Version 0.1.0.2
---------------

* Restore compatibility with GHC 7.4
* Fix a bug in `treeExtAny`, which previously work identically to
  `treeExtExists`
* Improve documentation

Version 0.1.0.1
---------------

Include CHANGELOG.md and README.md in the source distribution.

Version 0.1
-----------

* Allow to stop a listening job. Note this changes the return type of watching
  functions from `()` to `IO ()`.
* Previously, some care was taken to prevent multiple callbacks from running
  simultaneously. It is now the user's responsibility. See
  [#43](https://github.com/haskell-fswatch/hfsnotify/issues/43) for details.
* Previously, paths returned to callbacks were relative on Windows. Now they are
  absolute, like on the other platforms.
* The `WatchConfig` type has changed. Previously, it only specified debouncing
  parameters. Now it also contains polling parameters.
* The `isPollingManager` function is added to determine, at runtime, whether the
  polling implementation is used.
