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

Note: dump-core does not work for GHC-9.0.x, 9.6.x and 9.8.x.

`-fplugin-opt=Fusion.Plugin:dump-core`: dump core after each
core-to-core transformation. Output from each transformation is printed
in a different file.

`-fplugin-opt=Fusion.Plugin:verbose=1`: report unfused functions. Verbosity
levels `2`, `3`, `4` can be used for more verbose output.

### Reporting Programmer Annotated Binders

The `verbose=N` report above covers the whole module. To check a single
binding instead, annotate it with `Inspect` from `Fusion.Plugin.Types`
(from the `fusion-plugin-types` package). This works regardless of
`verbose=N` -- an `Inspect` annotated binding is always reported, even when
the plugin is otherwise silent:

```haskell
{-# LANGUAGE TemplateHaskellQuotes #-}

import Fusion.Plugin.Types
    (checkFusion, forbid, allow, forbidTypes, allowOnlyTypes)

-- checkFusion: Fuse-annotated types are forbidden by default, with an extra
-- forbid-list and an overriding allow-list.

-- Just enforce the baseline: nothing Fuse-annotated may survive to core.
{-# ANN function1 (checkFusion mempty) #-}
function1 :: ...

-- Also forbid Text even though it isn't Fuse-annotated.
{-# ANN function1a (checkFusion (forbid [''Text])) #-}
function1a :: ...

-- Forbid Text, but explicitly allow ByteString even if it's Fuse-annotated
-- or would otherwise be caught.
{-# ANN function1b (checkFusion (forbid [''Text] <> allow [''ByteString])) #-}
function1b :: ...

-- forbidTypes: blocklist -- only the named types are disallowed, everything
-- else in core is fine.
{-# ANN function2 (forbidTypes [''SomeType]) #-}
function2 :: ...

-- allowOnlyTypes: allowlist -- only the named types may appear, everything
-- else in core fails the check.
{-# ANN function3 (allowOnlyTypes [''Int, ''IO]) #-}
function3 :: ...
```

Type references are TH `Name`s (`''SomeType`), so a typo or a stale
reference to a renamed type is a compile error, not a silently-stale check.

## See also

If you are a library author looking to annotate the types, you need to
use the
[fusion-plugin-types](https://hackage.haskell.org/package/fusion-plugin-types)
package.

## Contributing

All contributions are welcome!  The code is available under Apache-2.0
license [on github](https://github.com/composewell/fusion-plugin).  In
case you have any questions or suggestions please contact [the
maintainers](mailto:streamly@composewell.com).

We would be happy to see this work getting integrated with GHC as a fix for
[GHC ticket #17075](https://gitlab.haskell.org/ghc/ghc/issues/17075), any help
with that would be appreciated.

## References

* [Stream Fusion: Practical shortcut fusion for coinductive sequence types](https://ora.ox.ac.uk/objects/uuid:b4971f57-2b94-4fdf-a5c0-98d6935a44da)
* [Stream Fusion From Lists to Streams to Nothing at All](https://www.researchgate.net/publication/221241130_Stream_Fusion_From_Lists_to_Streams_to_Nothing_at_All)
