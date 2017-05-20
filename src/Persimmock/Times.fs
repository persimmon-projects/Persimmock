(*
Copyright 2012 Phillip Trelford

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*)

namespace Persimmock

type internal ExpectedTimes =
    | Exactly of int
    | AtLeast of int
    | AtMost of int

/// Specifies valid invocation count
type Times internal (expected:ExpectedTimes) =     
    override __.ToString() =
        match expected with
        | Exactly num -> sprintf "exactly (=) %d" num
        | AtLeast num -> sprintf "at least (>=) %d" num
        | AtMost num -> sprintf "at most (<=) %d" num
    member internal times.Met(actual) =
        match expected with
        | Exactly expected -> actual = expected
        | AtLeast expected -> actual >= expected
        | AtMost expected -> actual <= expected
    member internal times.IsValid(actual) =
        match expected with
        | Exactly expected -> actual <= expected
        | AtLeast expected -> true
        | AtMost expected -> actual <= expected
    static member Exactly(n:int) = Times(Exactly(n))
    static member AtLeast(n:int) = Times(AtLeast(n))
    static member AtLeastOnce = Times.AtLeast(1)
    static member AtMost(n:int) = Times(AtMost(n))
    static member AtMostOnce = Times.AtMost(1)
    static member Never = Times.Exactly(0)
    static member Once = Times.Exactly(1)

[<AutoOpen;CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Times =
    let exactly = Times.Exactly
    let atleast = Times.AtLeast
    let atleastonce = Times.AtLeastOnce
    let atmost = Times.AtMost
    let atmostonce = Times.AtMostOnce
    let never = Times.Never
    let once = Times.Once
