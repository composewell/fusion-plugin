## 0.2.7

* Support ghc-9.6 and ghc-9.8

## 0.2.6

* Force inline on joins that consume fusible constructors

## 0.2.5

* Support ghc-9.4.2

## 0.2.4

* Suport ghc-9.2.2
* `dump-core` CLI flag support for ghc-9.2.x

## 0.2.3

* Enhancement: Support ghc-9.0.1
* Note: `dump-core` command line flag does not work for ghc-9.0.1

## 0.2.2

### Enhancements

- Plugin now forces inline on binders that return a type annotated with Fuse.
- `Fusion.Plugin` now accepts new command line arguments to control the
  verbosity of the plugin's reporting phase. Refer to `README.md` for usage.

## 0.2.1

### Enhancements

- `Fusion.Plugin` now accepts a command line argument `dump-core` to
  dump the core from each core-to-core pass in a separate file.

## 0.2.0

### Breaking Change
- Fusion.Plugin.Types is no longer exported from this package. If you
  want to annotate the types in your library, use the package
  fusion-plugin-types.

## 0.1.1

### Bug Fixes

- Now compiles successfully on old GHC versions till 7.10.3.

## 0.1.0

* Initial release
