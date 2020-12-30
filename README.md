# fusion-plugin

[![Hackage](https://img.shields.io/hackage/v/fusion-plugin.svg?style=flat)](https://hackage.haskell.org/package/fusion-plugin)
![](https://github.com/composewell/fusion-plugin/workflows/Haskell%20CI/badge.svg)


## Motivation

The goal of stream fusion is to eliminate constructors of
internal state used in a stream. For example, in case of
[streamly](https://github.com/composewell/streamly) streams, the
constructors of `Step` type, `Yield`, `Skip` and `Stop` would get
eliminated by fusion.  Similarly, constructors of any other intermediate
state types get eliminated when stream fusion works correctly. See the papers
in the reference section for more details on stream fusion.

Stream fusion depends on the GHC case-of-case transformations
eliminating intermediate constructors.  Case-of-case transformation in
turn depends on inlining. During core-to-core transformations GHC may
create several internal bindings (e.g. join points) which may not get
inlined because their size is too big. Even though we know that after
fusion the resulting code would be smaller and more efficient. The
programmer cannot force inlining of these bindings as there is no way
for the programmer to address these bindings at the source level because
they are internal, generated during core-to-core transformations. As a result
stream fusion fails unpredictably depending on whether GHC was able to inline
the internal bindings or not.

[See GHC ticket #17075](https://gitlab.haskell.org/ghc/ghc/issues/17075) for
more details.

## Solution

This plugin provides the programmer with a way to annotate certain
types using a `Fuse` pragma from the
[fusion-plugin-types](https://hackage.haskell.org/package/fusion-plugin-types)
package. The programmer would annotate the types that are to be
eliminated by fusion. During the simplifier phase the plugin goes
through the relevant bindings and if one of these types are found
inside a binding then that binding is marked to be inlined
irrespective of the size.

## Using the plugin

This plugin was primarily motivated by
[streamly](https://github.com/composewell/streamly) but it can be used in
general.

To use this plugin, add this package to your `build-depends`
and pass the following to your ghc-options:
`ghc-options: -O2 -fplugin=Fusion.Plugin`

### Plugin options

`-fplugin-opt=Fusion.Plugin:dump-core`: dump core after each
core-to-core transformation. Output from each transformation is printed
in a different file.

`-fplugin-opt=Fusion.Plugin:verbose=1`: report unfused functions. Verbosity
levels `2`, `3`, `4` can be used for more verbose output.

## See also

If you are a library author looking to annotate the types, you need to
use the
[fusion-plugin-types](https://hackage.haskell.org/package/fusion-plugin-types)
package.

## Contributing

All contributions are welcome!  The code is available under BSD-3
license [on github](https://github.com/composewell/fusion-plugin).  In
case you have any questions or suggestions please contact [Pranay
Sashank](mailto:pranaysashank@composewell.com), the author and
maintainer of this plugin.

We would be happy to see this work getting integrated with GHC as a fix for
[GHC ticket #17075](https://gitlab.haskell.org/ghc/ghc/issues/17075), any help
with that would be appreciated.

## References

* [Stream Fusion: Practical shortcut fusion for coinductive sequence types](https://pdfs.semanticscholar.org/b109/dc862d03c3ab0f3a2346e0685aa94e07f4c1.pdf)
* [Stream Fusion From Lists to Streams to Nothing at All](http://fun.cs.tufts.edu/stream-fusion.pdf)
