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

This plugin was primarily motivated by
[streamly](https://github.com/composewell/streamly) but it can be used in
general.

## Using the plugin

There are two different stages where the plugin functionality is used, (1) when
writing code for fusion we annotate data types or bindings using annotations
imported from the `fusion-plugin-types` packages, (2) when compiling the code
we use the `fusion-plugin` package as a compiler plugin to make those
annotations work towards fusing the code better.

## Annotations to Enable Fusion

See the `fusion-plugin-types` package for detailed reference on available
annotations.

### Enabling Fusion of a Data Type

Library authors annotate the data types used in the intermediate states of
stream pipelines using the `Fuse` annotation. All the functions where these
data types appear are liable to be force inlined by the plugin to make the
types fuse.

### Enabling Fusion within a Function

`Fuse` marks a type as fusible everywhere it is used. Sometimes a type should
drive fusion only inside one particular function and must not force inlining
wherever else it happens to be used. For that, annotate the *binding* (not the
type) with `FuseTypes` from `Fusion.Plugin.Types`. The listed types then behave
exactly as if they carried a `Fuse` annotation, but only while inlining inside
that one binding:

```haskell
{-# LANGUAGE TemplateHaskellQuotes #-}

import Fusion.Plugin.Types (FuseTypes(..))

{-# ANN function (FuseTypes [''Step, ''Maybe]) #-}
function :: ...
```

Type references are TH `Name`s (`''Step`), so a typo or a stale reference to a
renamed type is a compile error rather than a silently-ignored annotation.
Using `''Foo` requires `{-# LANGUAGE TemplateHaskellQuotes #-}` (or the heavier
`TemplateHaskell`) in the annotated module.

### Disabling Fusion within a Function

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

{-# ANN function (NoFuseTypes [''Step, ''Maybe]) #-}
function :: ...
```

`NoFuse` is the blanket form of `NoFuseTypes`: instead of listing types,
annotate the *binding* with `NoFuse` from `Fusion.Plugin.Types` to disable
forced inlining for that binding altogether, regardless of which types are
involved. This overrides any module-wide `Fuse` annotation and any
`FuseTypes`/`NoFuseTypes` on the same binding:

```haskell
import Fusion.Plugin.Types (NoFuse(..))

{-# ANN function NoFuse #-}
function :: ...
```

## Annotations to Verify Fusion

### Inspecting Boxed Types within a Function

To verify elimination of certain types within a function annotate the
function with `InspectTypes`. If a violation is found the plugin will complain
providing details of the violation. When `werror` plugin options is used it
will fail the compilation on violation.

```haskell
{-# LANGUAGE TemplateHaskellQuotes #-}

import Fusion.Plugin.Types (InspectTypes(..))
```

### Using `Fuse` Annotated as Baseline

Complain if any `Fuse` annotated type is found in the function.
```haskell
{-# ANN function (ForbidFused [] []) #-}
function1 :: ...
```

Also forbid `Maybe` as well even though it isn't Fuse-annotated.
```haskell
{-# ANN function (ForbidFused [''Maybe] []) #-}
function1a :: ...
```

Disallow `Maybe`, but explicitly allow `Step` even though it is Fuse-annotated
we allow it to be present in this binding.
```haskell
{-# ANN function (ForbidFused [''Maybe] [''Step]) #-}
function1b :: ...
```

### Forbidding Only Selected Types

Disallow the specified types and allow the rest, irrespective of `Fuse`
annotation. Both pattern macthes and allocations are checked.
```haskell
{-# ANN function (ForbidBoxedUse [''Step]) #-}
function2 :: ...
```

Report the listed types only where they are pattern-matched:
```haskell
{-# ANN function (ForbidPatternMatches [''Step]) #-}
```

Report the listed types only where they are allocated:
```haskell
{-# ANN function (ForbidAllocations [''Step]) #-}
```

### Permitting Only Selected Types

Allow only the specified types and disallow all others.
Both pattern macthes and allocations are checked.
```haskell
{-# ANN function (PermitBoxedUse [''Int, ''IO]) #-}
function3 :: ...
```

To report all boxed types used within a function:
```haskell
{-# ANN function (PermitBoxedUse []) #-}
function4 :: ...
```

Report every pattern-matched type except the listed ones:
```haskell
{-# ANN function (PermitPatternMatches [''Int, ''IO]) #-}
```

Report every allocated type except the listed ones:
```haskell
{-# ANN function (PermitAllocations [''Int, ''IO]) #-}
```

If any of the types in the permitted list is not actually found in the
binding, a warning is emitted so that stale entries can be removed from
the list.

### Inspecting type class dictionaries

To check the presence or absence of type classes in the Core of a binding,
annotate it with `InspectTypeClasses`. A type class appears in Core as a
dictionary argument; a class that survives to Core usually means a dictionary
that failed to specialize away.

```haskell
{-# LANGUAGE TemplateHaskellQuotes #-}

import Fusion.Plugin.Types (InspectTypeClasses(..))
```

'Num' should not appear in the core.
```haskell
{-# ANN function1 (ForbidTypeClasses [''Num]) #-}
function1 :: ...
```

'Ord' and 'Eq' can appear but no other type class can be present.
```haskell
{-# ANN function2 (PermitTypeClasses [''Ord, ''Eq]) #-}
function2 :: ...
```
If any of the permitted type classes is not actually found in the binding, a
warning is emitted so that stale entries can be removed from the list.

### Inspecting Core Size of a Function

Sometimes the Core blows up due to aggressive inlining and SpecConstr
optimizations. To guard against or detect such issues we can use the
`MaxCoreSize` as a core size ratchet for a function.

```haskell
import Fusion.Plugin.Types (MaxCoreSize(..))

{-# ANN function (MaxCoreSize 1000) #-}
function :: ...
```

This prints a line such as:
```
fusion-plugin: function: core size (1361 terms) exceeds the specified size (1000 terms).
```

### Generating Core of a Function

Use `DumpCore` annotation to write the final simplified core of a function to a
file in the compiler's dump directory (as set by `-dumpdir`), or under
`fusion-plugin-output/<package-name>` when no dump directory is set:
```haskell
{-# ANN function DumpCore #-}
function :: ...
```

To understand how the core of a function evolves through the optimizer, use the
`DumpCorePasses` annotation. It is the per-binding counterpart of the
`dump-core` option (see [Examining Optimization Passes](#examining-optimization-passes)).
Instead of dumping the whole module after each pass it dumps only the annotated
binding, writing one file per pass named
`<module>.<binding>.<NN-pass>.dump-simpl`:
```haskell
{-# ANN function DumpCorePasses #-}
function :: ...
```

## Compiling Code with the Plugin

To make the fusion annotations do the actual work you need to compile
your code with the fusion-plugin added to GHC during compilation. To do
that add this package to the `build-depends` in the cabal file of the
package to be compiled and use the following ghc options:
`ghc-options: -O2 -fplugin=Fusion.Plugin`

## Plugin options

### Finding fusion violations

`-fplugin-opt=Fusion.Plugin:verbose=1`: report unfused functions. Verbosity
levels `2`, `3`, `4` can be used for more verbose output.

`-fplugin-opt=Fusion.Plugin:werror`: treat annotation-check violations as
errors instead of warnings. This option is useful in CI builds to fail on
violation.

### Detecting code bloat

To record the core size of every binding that carries a violation-causing
annotation (`InspectTypes`, `InspectTypeClasses` or `MaxCoreSize`) to a file,
pass the `dump-core-sizes` option:
```
ghc-options: -fplugin-opt=Fusion.Plugin:dump-core-sizes
```
Each module then writes a `<module-name>.core-sizes.csv` file in the
compiler's dump directory (as set by `-dumpdir`), or in
`fusion-plugin-output/<package-name>` when no dump directory is set. The file
starts with a `name,core-size` header row followed by one row per annotated
binding.

This option is useful to collect the core sizes before and after a change and
automatically report the changes in a CI.

By default the CSV is truncated on each compilation. Pass `csv-append` to keep
the existing file and append each run's header and rows instead, accumulating
successive builds in one file:
```
ghc-options: -fplugin-opt=Fusion.Plugin:dump-core-sizes -fplugin-opt=Fusion.Plugin:csv-append
```

### Examining Optimization Passes

Note: dump-core does not work for GHC-9.0.x.

`-fplugin-opt=Fusion.Plugin:dump-core`: dump core after each
core-to-core transformation. Output from each transformation is printed
in a different file. This is useful to examine where an optimization failed or
occurred.

The files are written under the compiler's dump directory
as set by `-dumpdir`, or `fusion-plugin-output/<package-name>`
otherwise.  Each pass adds a `<NN-pass-name>.dump-simpl` suffix, e.g.
`Stream.Type.Basic.07-After-simplifier.dump-simpl`.

### Examining Before and After Core

`dump-core-if-annotated` option dumps the core of every binding
that carries a violation-causing annotation (`InspectTypes`,
`InspectTypeClasses` or `MaxCoreSize`), without adding a `DumpCore`
annotation to each one:
```
ghc-options: -fplugin-opt=Fusion.Plugin:dump-core-if-annotated
```

Use `dump-core-if-violated` to dump the core of those annotated
bindings for which a check actually reported a violation:
```
ghc-options: -fplugin-opt=Fusion.Plugin:dump-core-if-violated
```

This option is useful to examine the core to find the reported
violations. `dump-core-if-annotated` can be used in a baseline CI build
to dump cores of all annotated bindings, and the `dump-core-if-violated`
can be used to dump cores of violations in a changed build which can be
diffed against the baseline cores.

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
