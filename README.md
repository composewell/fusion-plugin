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

### Per-binding fusion with `FuseTypes`

`Fuse` marks a type as fusible everywhere it is used. Sometimes a type should
drive fusion only inside one particular function and must not force inlining
wherever else it happens to be used. For that, annotate the *binding* (not the
type) with `FuseTypes` from `Fusion.Plugin.Types`. The listed types then behave
exactly as if they carried a `Fuse` annotation, but only while inlining inside
that one binding:

```haskell
{-# LANGUAGE TemplateHaskellQuotes #-}

import Fusion.Plugin.Types (FuseTypes(..))

{-# ANN myFunction (FuseTypes [''Step, ''MyMaybe]) #-}
myFunction :: ...
```

Type references are TH `Name`s (`''Step`), so a typo or a stale reference to a
renamed type is a compile error rather than a silently-ignored annotation.
Using `''Foo` requires `{-# LANGUAGE TemplateHaskellQuotes #-}` (or the heavier
`TemplateHaskell`) in the annotated module.

### Per-binding override with `NoFuseTypes`

`NoFuseTypes` is the inverse of `Fuse`/`FuseTypes`. Sometimes a type should
drive fusion in general (via a module-wide `Fuse` annotation) but must *not*
force inlining inside one particular function. Annotate that *binding* with
`NoFuseTypes` from `Fusion.Plugin.Types`: the listed types then behave as if
they carried no `Fuse` annotation, disabling the forced inlining they would
otherwise drive, but only while inlining inside that one binding. Fusion of
those types everywhere else in the module is unaffected:

```haskell
{-# LANGUAGE TemplateHaskellQuotes #-}

import Fusion.Plugin.Types (NoFuseTypes(..))

{-# ANN myFunction (NoFuseTypes [''Step, ''MyMaybe]) #-}
myFunction :: ...
```

### Reporting Core size with `ShowCoreSize`

To find out how big the optimized Core of a particular binding is, annotate
that *binding* with `ShowCoreSize` from `Fusion.Plugin.Types`. After all
fusion-plugin passes have run, the plugin prints the Core size of that
binding, regardless of the module-wide `-fplugin-opt=...:verbose=N` flag. An
unexpected blow-up in size is often a symptom of failed fusion or runaway
inlining:

```haskell
import Fusion.Plugin.Types (ShowCoreSize(..))

{-# ANN myFunction ShowCoreSize #-}
myFunction :: ...
```

This prints a line such as:

```
fusion-plugin: Core size of myFunction: {terms: 8, types: 3, coercions: 0, joins: 0/0}
```

### Plugin options

Note: dump-core does not work for GHC-9.0.x, 9.6.x and 9.8.x.

`-fplugin-opt=Fusion.Plugin:dump-core`: dump core after each
core-to-core transformation. Output from each transformation is printed
in a different file.

`-fplugin-opt=Fusion.Plugin:verbose=1`: report unfused functions. Verbosity
levels `2`, `3`, `4` can be used for more verbose output.

`-fplugin-opt=Fusion.Plugin:werror`: treat annotation-check violations as
errors instead of warnings.

### Reporting Programmer Annotated Binders

The `verbose=N` report above covers the whole module. To check a single
binding instead, annotate it with `InspectTypes` from `Fusion.Plugin.Types`
(from the `fusion-plugin-types` package). This works regardless of
`verbose=N` -- an `InspectTypes` annotated binding is always reported, even when
the plugin is otherwise silent:

```haskell
{-# LANGUAGE TemplateHaskellQuotes #-}

import Fusion.Plugin.Types (InspectTypes(..))

-- ForbidFused: Fuse-annotated types are forbidden by default, with an
-- extra forbid-list and an overriding allow-list.

-- Just enforce the baseline: nothing Fuse-annotated may survive to core.
{-# ANN function1 (ForbidFused [] []) #-}
function1 :: ...

-- Also forbid Maybe even though it isn't Fuse-annotated.
{-# ANN function1a (ForbidFused [''Maybe] []) #-}
function1a :: ...

-- Forbid Maybe, but explicitly allow Step even though it's Fuse-annotated
-- we allow it to be present in this binding.
{-# ANN function1b (ForbidFused [''Maybe] [''Step]) #-}
function1b :: ...

-- ForbidTypes: blocklist -- only the named types are disallowed, everything
-- else in core is fine.
{-# ANN function2 (ForbidTypes [''Step]) #-}
function2 :: ...

-- PermitTypes: allowlist -- only the named types may appear, everything
-- else in core fails the check.
{-# ANN function3 (PermitTypes [''Int, ''IO]) #-}
function3 :: ...
```

Type references are TH `Name`s (`''SomeType`), so a typo or a stale
reference to a renamed type is a compile error, not a silently-stale check.

### Checking type classes with `InspectTypeClasses`

To check the presence or absence of type classes in the Core of a binding,
annotate it with `InspectTypeClasses`. A type class appears in Core as a
dictionary argument; a class that survives to Core usually means a dictionary
that failed to specialize away. Like `InspectTypes`, an annotated binding is
always reported regardless of `verbose=N`:

```haskell
{-# LANGUAGE TemplateHaskellQuotes #-}

import Fusion.Plugin.Types (InspectTypeClasses(..))

-- 'Num' should not appear in the core.
{-# ANN function1 (ForbidTypeClasses [''Num]) #-}
function1 :: ...

-- 'Ord' and 'Eq' can appear but no other type class can be present.
{-# ANN function2 (PermitTypeClasses [''Ord, ''Eq]) #-}
function2 :: ...
```

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
