/// Hot-loop helpers. Fable 5 compiles each `a[i]` to a bounds-checked library call,
/// which made training 17x slower in the browser than on .NET.
module SnakeEvo.Native

#if FABLE_COMPILER
open Fable.Core

/// Unchecked array read. Callers guarantee the index is in range.
[<Emit("$1[$0]")>]
let at (i: int) (a: 'T[]) : 'T = jsNative
#else
let inline at (i: int) (a: 'T[]) = a[i]
#endif
