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

open System
open System.Reflection

/// Mock object interface for verification
type IMockObject =
    abstract Invocations : Invocations
    [<CLIEvent>]
    abstract Invoked : IEvent<EventHandler,EventArgs>
    abstract Verifiers : Verifiers
/// Member invocation record
and Invocation = { Method : MethodBase; Args : obj[] }
/// List of invocations
and Invocations = Invocation list
/// List of verifiers
and Verifiers = System.Collections.Generic.List<Action>

/// Mock recorder interface
type IMockRecorder =
    abstract Reset : unit -> unit
    abstract Add : Invocation -> unit

/// Mock mode
type MockMode = Strict = 0 | Loose = 1

module MockMode =
    /// Default mock mode
    let mutable Default = MockMode.Loose

/// Wildcard attribute
[<AttributeUsage(AttributeTargets.Method ||| AttributeTargets.Property)>]
type WildcardAttribute() = inherit Attribute()

/// Predicate attribute
[<AttributeUsage(AttributeTargets.Method)>]
type PredicateAttribute() = inherit Attribute()

/// Returns attribute
[<AttributeUsage(AttributeTargets.Method)>]
type ReturnsAttribute() = inherit Attribute()

/// Calls attribute
[<AttributeUsage(AttributeTargets.Method)>]
type CallsAttribute() = inherit Attribute()

/// Raises attribute
[<AttributeUsage(AttributeTargets.Method)>]
type RaisesAttribute() = inherit Attribute()
