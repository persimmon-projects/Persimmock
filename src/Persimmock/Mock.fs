namespace Persimmock

open System
open System.Reflection
open FSharp.Quotations
open FSharp.Quotations.Patterns
open Emit
open QuotationEvaluation
open Persimmock.Reflection

type Mock<'TAbstract when 'TAbstract : not struct> 
  internal (mode: MockMode,
            otherTypes: Type list,
            calls: (MethodInfo * (Arg[] * Result)) list,
            returnStrategy: (Type -> obj) option) =

  let abstractType = typeof<'TAbstract>

  new () = Mock(MockMode.Default, [], [], None)
  new (?returnStrategy) = Mock(MockMode.Default, [], [], returnStrategy)
  new (mode, ?returnStrategy) = Mock(mode, [], [], returnStrategy)

  member internal __.Type = abstractType
  member internal __.Mode = mode
  member internal __.OtherTypes = otherTypes
  member internal __.Calls = calls
  member internal __.ReturnStrategy = returnStrategy

  member __.Create([<ParamArray>] args: obj[]) = 
    mock(mode, abstractType, otherTypes, calls, args, returnStrategy) :?> 'TAbstract

  member internal __.Setup<'TReturnValue>(call) =
    ResultBuilder<'TAbstract, 'TReturnValue>(mode, otherTypes, call, calls, returnStrategy)
  member internal __.SetupMethod<'TArgs, 'TReturnValue>(call) =
    MethodBuilder<'TAbstract, 'TArgs, 'TReturnValue>(mode, otherTypes, call, calls, returnStrategy)
  member internal __.SetupEvent<'TEvent>(handlers) =
    EventBuilder<'TAbstract, 'TEvent>(mode, otherTypes, handlers, calls, returnStrategy)

and IMemberBuilder<'TAbstract,'TReturnValue when 'TAbstract : not struct> =
  abstract member Raises: exn -> Mock<'TAbstract>

and ResultBuilder<'TAbstract,'TReturnValue when 'TAbstract : not struct> 
  internal (mode, otherTypes, call, calls, returnStrategy) =
  
  let mi, args = call

  member __.Returns(f:unit -> 'TReturnValue) =
    let result = 
      if typeof<'TReturnValue> = typeof<unit> then Unit
      else ReturnFunc(f, typeof<'TReturnValue>)
    let call = mi, (args, result)
    Mock<'TAbstract>(mode, otherTypes, call :: calls, returnStrategy)

  [<RequiresExplicitTypeArguments>]
  member __.Calls<'TArgs>(f:'TArgs -> 'TReturnValue) =
    let call = mi, (args, Call(f, typeof<'TArgs>, typeof<'TReturnValue>))
    Mock<'TAbstract>(mode, otherTypes, call :: calls, returnStrategy)

  interface IMemberBuilder<'TAbstract, 'TReturnValue> with
    member __.Raises(ex: exn): Mock<'TAbstract> =
      let call = mi, (args, RaiseValue(ex))
      Mock<'TAbstract>(mode, otherTypes, call :: calls, returnStrategy)

and MethodBuilder<'TAbstract, 'TArgs, 'TReturnValue when 'TAbstract : not struct> 
  internal (mode, otherTypes, call, calls, returnStrategy) =

  let mi, args = call

  member internal __.Apply(args: Expr seq) =
    let call: MethodInfo * Arg [] = (mi, toArgs (mi.GetParameters()) args)
    ResultBuilder<_, _>(mode, otherTypes, call, calls, returnStrategy)

  member __.Calls(f:'TArgs -> 'TReturnValue) =
    let result =
      if typeof<'TArgs> = typeof<unit> then ReturnFunc(f, typeof<'TReturnValue>)
      else Call(f, typeof<'TArgs>, typeof<'TReturnValue>)
    let call = mi, (args, result)
    Mock<'TAbstract>(mode, otherTypes, call :: calls, returnStrategy)

  interface IMemberBuilder<'TAbstract, 'TReturnValue> with
    member __.Raises(ex: exn): Mock<'TAbstract> =
      let call = mi, (args, RaiseValue(ex))
      Mock<'TAbstract>(mode, otherTypes, call :: calls, returnStrategy)

and EventBuilder<'TAbstract,'TEvent when 'TAbstract : not struct> 
  internal (mode, otherTypes, handlers, calls, returnStrategy) =
  
  let add, remove = handlers
  
  member __.Publishes(value:'TEvent) =
    let add = add, ([|Any|], Handler("AddHandler",value))
    let remove = remove, ([|Any|], Handler("RemoveHandler",value))
    Mock<'TAbstract>(mode, otherTypes, add :: remove :: calls, returnStrategy)

type MockBuilder<'TAbstract when 'TAbstract : not struct>() =

  let argsToSeq = function
  | WithValue(_, _, NewTuple(args))
  | NewTuple(args) -> args
  | WithValue(_, _, arg) -> [arg]
  | e -> raise <| NotSupportedException(sprintf "Expected new tuple, got: %A" e)

  let argToValue = function
  | Value(v, _)
  | ValueWithName(v, _, _)
  | WithValue(v, _, _) -> v
  | e -> raise <| ArgumentException(sprintf "Expected Value, ValueWithName, or WithValue, got: %A" e)

  let argsToArray = function
  | WithValue(_, _, NewTuple(args))
  | NewTuple(args) -> args |> List.toArray |> Array.map (argToValue)
  | WithValue(_, _, v) -> [|argToValue v|]
  | e -> raise <| ArgumentException(sprintf "Expected new tuple, got: %A" e)

  member inline __.Zero() = Mock<'TAbstract>()
  member this.Yield(()) = this.Zero()

  [<CustomOperation("mode")>]
  member __.Mode(m: Mock<'TAbstract>, value) =
    Mock<'TAbstract>(value, m.OtherTypes, m.Calls, m.ReturnStrategy)

  [<CustomOperation("strategy")>]
  member __.Strategy(m: Mock<'TAbstract>, returnStrategy) =
    Mock<'TAbstract>(m.Mode, m.OtherTypes, m.Calls, Some returnStrategy)

  [<CustomOperation("setup")>]
  member __.Setup(m: Mock<'TAbstract>, [<ReflectedDefinition(true)>] f:Expr<'TAbstract -> 'TReturnValue>) =
    let e = unwrapLambda f
    let call = toCallOf m.Type e
    m.Setup<'TReturnValue>(call)

  [<CustomOperation("property")>]
  member __.SetupProperty(m: Mock<'TAbstract>, [<ReflectedDefinition(true)>] f:Expr<'TAbstract -> 'TReturnValue>) =
    let e = unwrapLambda f
    let call = toCallOf m.Type e
    m.Setup<'TReturnValue>(call)

  [<CustomOperation("returns")>]
  member __.Returns(builder: ResultBuilder<'TAbstract, 'TReturnValue>, f: unit -> 'TReturnValue) =
    builder.Returns(f)

  [<CustomOperation("returnUnit")>]
  member __.ReturnUnit(builder: ResultBuilder<'TAbstract, unit>) =
    builder.Returns(ignore)

  [<CustomOperation("hook")>]
  member __.Hook<'TArgs, 'TReturnValue>(builder: ResultBuilder<'TAbstract, 'TReturnValue>, f: 'TArgs -> 'TReturnValue) =
    builder.Calls<'TArgs>(f)

  [<CustomOperation("method")>]
  member __.SetupMethod(m: Mock<'TAbstract>, [<ReflectedDefinition(true)>] f: Expr<'TAbstract -> 'TArgs -> 'TReturnValue>) =
    let e = unwrapLambda f
    let call = toCallOf m.Type e
    m.SetupMethod<'TArgs, 'TReturnValue>(call)

  [<CustomOperation("args")>]
  member __.ApplyArgs(builder: MethodBuilder<'TAbstract, 'TArgs, 'TReturnValue>, [<ReflectedDefinition(true)>] args: Expr<'TArgs>) =
    builder.Apply(argsToSeq args)

  [<CustomOperation("call")>]
  member __.Calls(builder: MethodBuilder<'TAbstract, 'TArgs, 'TReturnValue>, f: 'TArgs -> 'TReturnValue) =
    builder.Calls(f)

  [<CustomOperation("raise")>]
  member __.Raises(builder: IMemberBuilder<'TAbstract, 'TReturnValue>, ex) =
    builder.Raises(ex)

  [<CustomOperation("event")>]
  member __.SetupEvent(m: Mock<'TAbstract>, [<ReflectedDefinition(true)>] f:Expr<'TAbstract -> 'TEvent>) =
    m.SetupEvent<'TEvent>(unwrapLambda f |> toHandlers m.Type)

  [<CustomOperation("publish")>]
  member __.Publishes(builder: EventBuilder<'TAbstract, 'TEvent>, value: 'TEvent) =
    builder.Publishes(value)

  [<CustomOperation("initialize")>]
  member __.Constructor(m: Mock<'TAbstract>, [<ReflectedDefinition(true)>]args: Expr<'TArgs>) =
    m.Create(argsToArray args)

  member __.Delay(f: unit -> _) = f
  member __.Run(f: unit -> 'TAbstract) =
    f ()
  member __.Run(f: unit -> Mock<'TAbstract>) =
    (f ()).Create()
