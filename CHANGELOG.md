Changes
=======

Version 0.4.0.0
---------------

API breaking update.

* New options for threading control (single-threaded, thread-per-watch, and thread-per-manager)
* Revamp `WatchConfig` options to be less confusing reduce boolean blindness.
* Pull out debouncing stuff, since it was never correct.
* Don't silently fall back to polling on failure of native watcher.
  Instead, throw an exception which the user can recover from by switching to polling.
* Add ModifiedAttributes event type + Linux support
* Add confOnHandlerException to be able to control what happens when a handler throws an exception.

Version 0.3.0.0
---------------

API breaking update with a number of bugfixes and improvements.

* Now we can detect directory creation/deletion. A boolean flag has been added
  to `Event` to indicate if the event pertains to a directory or not. This is the
  only API change.
* Test stability improvements + CI test suites now passing on Windows, Linux, and Mac.
* Interpreting OSX hfsevents flags is more sane now (see comments in OSX.hs for details).
* Improve a race condition when adding watches on Linux.
* Improve robustness of the PollManager.
* Fix double call to `closeHandle` on Windows.
* Remove comments about locking from the documentation.

Version 0.2.1.2
---------------

Update to the new hinotify API (v0.3.10)

Version 0.2.1.1
---------------

Catch IO exceptions when initialising inotify on Linux

Version 0.2.1
-------------

Don't use `system-filepath`

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
