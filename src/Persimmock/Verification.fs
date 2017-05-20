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
open FSharp.Reflection
open FSharp.Quotations
open Emit
open Persimmock.Reflection

module internal Verification =
    /// Returns true if method parameter types match
    let paramTypesMatch (a:MethodBase) (b:MethodBase) =
        let b = 
            if b.ContainsGenericParameters &&
               b.IsGenericMethod &&
               a.GetGenericArguments().Length = b.GetGenericArguments().Length 
            then            
                let mi = b :?> MethodInfo
                mi.MakeGenericMethod(a.GetGenericArguments()) :> MethodBase
            else b
        Array.zip (a.GetParameters()) (b.GetParameters())
        |> Array.forall (fun (a,b) -> a.ParameterType = b.ParameterType)
    /// Return true if methods match
    let methodsMatch (a:MethodBase) (b:MethodBase) =
        a.Name = b.Name &&
        a.GetParameters().Length = b.GetParameters().Length &&
        paramTypesMatch a b
    /// Returns true if arguments match
    let rec argsMatch (argType:Type) expectedArg (actualValue:obj) =
        match expectedArg with
        | Any -> true
        | Arg(expected) ->
            if argType.IsArray
            then expected = actualValue
            else obj.Equals(expected,actualValue)
        | ArgArray(expected) ->
            Array.zip expected (actualValue :?> obj[])
            |> Array.forall (fun (arg,value) -> argsMatch typeof<obj> arg value)
        | OutArg(_) -> true
        | Pred(p) ->
            let f = FSharpType.MakeFunctionType(argType,typeof<bool>).GetMethod("Invoke")
            f.Invoke(p,[|actualValue|]) :?> bool
        | PredUntyped(p) -> raise <| NotSupportedException("Predicate arguments cannot be used as expected arguments")
    let invokeMatch (expectedMethod:MethodBase) (expectedArgs:Arg[]) (actual:Invocation) =
        let ps = expectedMethod.GetParameters()
        methodsMatch expectedMethod actual.Method &&
        Array.zip expectedArgs actual.Args
        |> Array.mapi (fun i (e,a) -> ps.[i].ParameterType,e,a)
        |> Array.forall (fun (t,e,a) -> argsMatch t e a)
    /// Returns invocation count matching specificed expression
    let countInvocations (mock:IMockObject) (expectedMethod) (expectedArgs) =
        mock.Invocations
        |> List.filter (invokeMatch expectedMethod expectedArgs)
        |> List.length
    let getMock (x:obj) =
        match x with
        | :? IMockObject as mock -> mock
        | _ -> failwith "Object instance is not a mock"

open Eval
open Verification

[<AutoOpen>]
module private Format =
    let invoke (mi:MethodBase, args:obj seq) =
        let args = args |> Seq.map (sprintf "%O")
        mi.Name + "(" + (String.concat ", " args) + ")"
    let expected (mi:MethodBase, args:Arg[]) =
        let args = args |> Seq.map (function Arg x -> x | _ -> box "_") 
        invoke (mi, args)
    let unexpected (expectedMethod, expectedArgs, invocation:Invocation) =
        "Unexpected member invocation\r\n" +
        "Expected: " + expected(expectedMethod,expectedArgs) + "\r\n" +
        "Actual:   " + invoke(invocation.Method,invocation.Args)

type Mock private () =
    /// Verifies expected call count against instance member invocations on specified mock
    static member Verify([<ReflectedDefinition(false)>] expr:Expr<_>, expectedTimes:Times) =
        let target,expectedMethod,expectedArgs = toCall expr
        let mock = target |> eval |> getMock
        let actualCalls = countInvocations mock expectedMethod expectedArgs
        if not <| expectedTimes.Met(actualCalls) then
            let invocations =
                mock.Invocations
                |> List.rev
                |> List.mapi (fun index invocation ->
                    sprintf "%d. %s" (index + 1) (invoke(invocation.Method, invocation.Args))
                    )
                |> String.concat "\n"

            let actual =
                if mock.Invocations.Length > 0
                then sprintf "\nActual:\n%s" invocations
                else ""

            failwithf "Expected %O calls that match the expected pattern, but saw %d.\nExpected: %s%s" expectedTimes actualCalls (expected(expectedMethod, expectedArgs)) actual
    /// Verifies expression was invoked at least once
    static member Verify(expr:Expr) = Mock.Verify(expr, atleastonce)
    /// Verifies expected expression call count on invocation
    static member Expect([<ReflectedDefinition(false)>] expr:Expr<_>, expectedTimes:Times) =
        let target,expectedMethod,expectedArgs = toCall expr
        let mock = target |> eval |> getMock
        let expect f =
            let actualCalls = countInvocations mock expectedMethod expectedArgs
            if not <| f actualCalls then 
                failwith <| expected(expectedMethod,expectedArgs)
        mock.Invoked.Subscribe(fun _ ->
            let last = mock.Invocations |> List.head
            if invokeMatch expectedMethod expectedArgs last then expect (expectedTimes.IsValid)
            ) |> ignore
        mock.Verifiers.Add(Action(fun () -> expect (expectedTimes.Met)))
    // Verify call sequence in order
    static member VerifySequence([<ReflectedDefinition(false)>] expr:Expr<_>) =
        let calls = toCallResult expr
        let mocks = System.Collections.Generic.Dictionary()
        for target, expectedMethod,(expectedArgs,result) in calls do
            let mock = eval target |> getMock
            let invocations = mock.Invocations |> List.rev
            if not <| mocks.ContainsKey mock then mocks.Add(mock,0)
            let n = mocks.[mock]
            if invocations.Length = n then
                failwith <| "Missing expected member invocation: " + expected(expectedMethod,expectedArgs)
            let actual = invocations.[n]
            if not <| invokeMatch expectedMethod expectedArgs actual then
                failwith  <| unexpected(expectedMethod,expectedArgs,actual)
            mocks.[mock] <- n + 1
        for pair in mocks do
            let mock, n = pair.Key, pair.Value 
            let invocations = mock.Invocations |> List.rev
            if invocations.Length > n then 
                let last = invocations.[n]
                failwith <| "Unexpected member invocation: " + invoke(last.Method, last.Args)
    /// Verifies all expectations
    static member VerifyAll(mock:obj) =
        let mock = mock :?> IMockObject
        for verify in mock.Verifiers do verify.Invoke()