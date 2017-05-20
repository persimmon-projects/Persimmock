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

(*

* ChangeLog
  * remove POWERPACK
  * change Reflection module accessibility(private to internal)
  * rename dynamic assembly module
  * allow WithValue in unwrapLambda
  * allow ValueWithName in unwrapStandard

*)

// copy from https://github.com/fsprojects/Foq/blob/b3a7626146cd84a1fd60b300a25738b7e4e0a9d2/Foq/Foq.fs

namespace Persimmock

open System
open System.Reflection
open Microsoft.FSharp.Reflection

module internal Emit =
    open System.Reflection.Emit
    /// Boxed value
    type Value = obj
    /// Boxed function
    type Func = obj
    /// Boxed event
    type PublishedEvent = obj
    /// Method argument type
    type Arg = 
        | Any 
        | Arg of Value 
        | ArgArray of Arg[] 
        | OutArg of Value 
        | Pred of Func 
        | PredUntyped of Func

    /// Method result type
    type Result = 
        | Unit
        | ReturnValue of Value * Type
        | ReturnFunc of Func * Type
        | Handler of string * PublishedEvent
        | Call of Func * Type * Type
        | Raise of Type
        | RaiseValue of exn

    /// Generates constructor
    let generateConstructor (typeBuilder:TypeBuilder) ps (genBody:ILGenerator -> unit) =
        let cons = typeBuilder.DefineConstructor(MethodAttributes.Public,CallingConventions.Standard,ps)
        let il = cons.GetILGenerator()
        // Generate body
        genBody il
        il.Emit(OpCodes.Ret)

    /// Defines method
    let defineMethod (typeBuilder:TypeBuilder) (abstractMethod:MethodInfo) =
        let attr =
            MethodAttributes.Public ||| MethodAttributes.HideBySig ||| MethodAttributes.Virtual
        let args = abstractMethod.GetParameters() |> Array.map (fun arg -> arg.ParameterType)
        let m = typeBuilder.DefineMethod(abstractMethod.Name, attr, abstractMethod.ReturnType, args)
        if abstractMethod.IsGenericMethod then
            let names = abstractMethod.GetGenericArguments() |> Array.map (fun x -> x.Name)
            m.DefineGenericParameters(names) |> ignore
        m

    /// Generates method overload args match
    let generateArgs 
        (il:ILGenerator) (argsLookup:ResizeArray<Value[]>,argsField:FieldBuilder) 
        (mi:MethodInfo,args) (unmatched:Label) =
        /// Index of argument values for current method overload
        let argsLookupIndex = argsLookup.Count
        let rec toValue = function 
            | Any -> null 
            | Arg(value) -> value 
            | ArgArray(args) -> box [|for arg in args -> toValue arg|] 
            | OutArg(value) -> value 
            | Pred(f) -> f | PredUntyped(f) -> f
        // Add arguments to lookup
        args |> Array.map toValue |> argsLookup.Add
        // Emit argument matching
        let rec emitArg arrayIndex argIndex arg =
            let atIndex () =
                match arrayIndex with
                | Some(i:int) ->  
                    il.Emit(OpCodes.Ldc_I4, i)
                    il.Emit(OpCodes.Ldelem_Ref)
                | None -> ()
            let emitLdargBox () =
                il.Emit(OpCodes.Ldarg, argIndex+1)
                let pi = mi.GetParameters().[argIndex]
                il.Emit(OpCodes.Box, pi.ParameterType)
            let emitArgLookup () =
                il.Emit(OpCodes.Ldarg_0)
                il.Emit(OpCodes.Ldfld, argsField)
                il.Emit(OpCodes.Ldc_I4, argsLookupIndex)
                il.Emit(OpCodes.Ldelem_Ref)
                il.Emit(OpCodes.Ldc_I4, argIndex)
                il.Emit(OpCodes.Ldelem_Ref)
            match arg with
            | Any -> ()
            | Arg(value) ->
                emitLdargBox ()
                atIndex ()       
                emitArgLookup ()
                atIndex ()
                il.EmitCall(OpCodes.Call, typeof<obj>.GetMethod("Equals",[|typeof<obj>;typeof<obj>|]), null) 
                il.Emit(OpCodes.Brfalse, unmatched)
            | ArgArray(args) ->
                emitLdargBox ()
                il.Emit(OpCodes.Ldlen)
                emitArgLookup ()
                il.Emit(OpCodes.Ldlen)
                il.Emit(OpCodes.Ceq)
                il.Emit(OpCodes.Brfalse, unmatched)
                args |> Array.iteri (fun i arg -> emitArg (Some i) argIndex arg)
            | OutArg(value) ->
                il.Emit(OpCodes.Ldarg, argIndex+1)
                emitArgLookup ()
                let pi = mi.GetParameters().[argIndex]
                let t = pi.ParameterType.GetElementType()
                il.Emit(OpCodes.Unbox_Any, t)
                il.Emit(OpCodes.Stobj, t)
            | Pred(f) ->
                emitArgLookup ()
                atIndex()
                il.Emit(OpCodes.Ldarg, argIndex+1)
                atIndex()
                let argType = mi.GetParameters().[argIndex].ParameterType
                let invoke = FSharpType.MakeFunctionType(argType,typeof<bool>).GetMethod("Invoke")
                il.Emit(OpCodes.Callvirt, invoke)
                il.Emit(OpCodes.Brfalse, unmatched)
            | PredUntyped(f) ->
                emitArgLookup ()
                atIndex()
                il.Emit(OpCodes.Ldarg, argIndex+1)
                atIndex()
                let argType = mi.GetParameters().[argIndex].ParameterType
                il.Emit(OpCodes.Box, argType)
                let invoke = FSharpType.MakeFunctionType(typeof<obj>,typeof<bool>).GetMethod("Invoke")
                il.Emit(OpCodes.Callvirt, invoke)
                il.Emit(OpCodes.Brfalse, unmatched)
        
        if mi.IsGenericMethod then
            let concreteArgs = mi.GetGenericArguments()
            let genericArgs = mi.GetGenericMethodDefinition().GetGenericArguments()
            Array.zip concreteArgs genericArgs
            |> Array.iter (fun (arg,arg') ->
                let typeFromHandle = typeof<Type>.GetMethod("GetTypeFromHandle", [|typeof<RuntimeTypeHandle>|])
                il.Emit(OpCodes.Ldtoken, arg)
                il.Emit(OpCodes.Call, typeFromHandle)
                il.Emit(OpCodes.Ldtoken, arg')
                il.Emit(OpCodes.Call, typeFromHandle)
                il.Emit(OpCodes.Ceq)
                il.Emit(OpCodes.Brfalse, unmatched)
            )
        
        args |> Seq.iteri (emitArg None)

    /// Generates method return
    let generateReturn
        (il:ILGenerator) (returnValues:ResizeArray<Value>,returnValuesField:FieldBuilder) (mi:MethodInfo,result) =
        // Emits _returnValues.[returnValuesIndex]
        let emitReturnValueLookup value =
            let returnValuesIndex = returnValues.Count
            returnValues.Add(value)
            il.Emit(OpCodes.Ldarg_0)
            il.Emit(OpCodes.Ldfld, returnValuesField)
            il.Emit(OpCodes.Ldc_I4, returnValuesIndex)
            il.Emit(OpCodes.Ldelem_Ref)
        /// Emits AddHandler/RemoveHandler
        let emitEventHandler handlerName e =
            emitReturnValueLookup e
            let handlerType = e.GetType().GetGenericArguments().[0]
            il.Emit(OpCodes.Ldarg_1)
            let t = typedefof<IDelegateEvent<_>>.MakeGenericType(handlerType)
            let invoke = t.GetMethod(handlerName)
            il.Emit(OpCodes.Callvirt, invoke)
            il.Emit(OpCodes.Ret)
        /// Emits return value with out arguments
        let emitReturnWithOutArgs emitGetValue returnType =
            let ps = mi.GetParameters()
            if FSharpType.IsTuple returnType then
                let local = il.DeclareLocal(returnType).LocalIndex
                emitGetValue ()
                il.Emit(OpCodes.Stloc, local)
                let emitGetItem x =
                    il.Emit(OpCodes.Ldloc, local)
                    let name = sprintf "Item%d" (x+1)
                    let item = returnType.GetProperty(name).GetGetMethod()
                    il.Emit(OpCodes.Callvirt, item)
                let isVoid = mi.ReturnType = typeof<Void>
                let startIndex = if isVoid then 0 else 1
                let items = FSharpType.GetTupleElements returnType
                for index = startIndex to items.Length - 1 do
                    let paramIndex = ps.Length - items.Length + index
                    il.Emit(OpCodes.Ldarg, paramIndex + 1)
                    emitGetItem index
                    let t = ps.[paramIndex].ParameterType.GetElementType()
                    il.Emit(OpCodes.Stobj, t)
                if not isVoid then
                    emitGetItem 0
            else
                il.Emit(OpCodes.Ldarg, ps.Length-1+1)
                emitGetValue ()
                let t = ps.[ps.Length-1].ParameterType.GetElementType()
                il.Emit(OpCodes.Stobj, t)
        // Emit result
        match result with
        | Unit -> il.Emit(OpCodes.Ret)
        | ReturnValue(value, returnType) ->
            if returnType <> mi.ReturnType then
                let emitGetValue () = 
                    emitReturnValueLookup value
                    il.Emit(OpCodes.Unbox_Any, returnType)
                emitReturnWithOutArgs emitGetValue returnType
            else
                emitReturnValueLookup value
                il.Emit(OpCodes.Unbox_Any, returnType)
            il.Emit(OpCodes.Ret)
        | ReturnFunc(f, returnType) ->
            let emitGetValue () =
                emitReturnValueLookup f
                // Emit Invoke
                il.Emit(OpCodes.Ldnull)
                let invoke = FSharpType.MakeFunctionType(typeof<unit>,returnType).GetMethod("Invoke")
                il.Emit(OpCodes.Callvirt, invoke)
            if returnType <> mi.ReturnType then
                emitReturnWithOutArgs emitGetValue returnType
            else
                emitGetValue ()
                if mi.ReturnType = typeof<unit> || mi.ReturnType = typeof<Void> then 
                    il.Emit(OpCodes.Pop)
            il.Emit(OpCodes.Ret)
        | Handler(handlerName, e) -> emitEventHandler handlerName e
        | Call(f, argsType, returnType) ->
            emitReturnValueLookup f
            // Emit args
            let ps = mi.GetParameters()
            let args =
               if FSharpType.IsTuple argsType then FSharpType.GetTupleElements(argsType)
               else [|argsType|]
            let emitArgs () =
               // Load args converting out or byref args to value
               for i = 0 to args.Length-1 do
                  il.Emit(OpCodes.Ldarg, i+1)
                  let pi = ps.[i]
                  if pi.IsOut || pi.ParameterType.IsByRef then
                     il.Emit(OpCodes.Ldobj, args.[i])
            let emitArray () =
               il.Emit(OpCodes.Ldc_I4, args.Length)
               il.Emit(OpCodes.Newarr, typeof<obj>)
               for i = 0 to args.Length-1 do
                  il.Emit(OpCodes.Dup)
                  il.Emit(OpCodes.Ldc_I4, i)
                  il.Emit(OpCodes.Ldarg, i+1)
                  il.Emit(OpCodes.Box, ps.[i].ParameterType)
                  il.Emit(OpCodes.Stelem_Ref)
            // Construct args tuple
            if FSharpType.IsTuple argsType then
               let tuple = FSharpType.MakeTupleType(args)
               if args.Length < 8 then
                  emitArgs ()
                  let ci = tuple.GetConstructor(args)
                  il.Emit(OpCodes.Newobj, ci)
               else
                  emitArray ()
                  il.Emit(OpCodes.Ldtoken, tuple)
                  let t = typeof<Type>.GetMethod("GetTypeFromHandle", [|typeof<RuntimeTypeHandle>|])
                  il.Emit(OpCodes.Call, t)
                  let mi = typeof<FSharpValue>.GetMethod("MakeTuple")
                  il.Emit(OpCodes.Call, mi)
            else emitArgs ()
            // Function call
            let invoke = FSharpType.MakeFunctionType(argsType, returnType).GetMethod("Invoke")
            il.Emit(OpCodes.Callvirt, invoke)
            // Handle return value
            if mi.ReturnType = typeof<unit> || mi.ReturnType = typeof<Void> then 
                il.Emit(OpCodes.Pop)
            elif returnType <> mi.ReturnType then
                emitReturnWithOutArgs ignore returnType            
            il.Emit(OpCodes.Ret)
        | Raise(exnType) -> il.ThrowException(exnType)
        | RaiseValue(exnValue) ->
            emitReturnValueLookup exnValue
            il.Emit(OpCodes.Throw)

    /// Generates new invocation from current method on stack
    let generateNewInvocation 
        (il:ILGenerator) (invocationsField:FieldBuilder) (abstractMethod:MethodInfo) =
        let ps = abstractMethod.GetParameters()
        // Create local array to store arguments
        let localArray = il.DeclareLocal(typeof<obj[]>).LocalIndex
        il.Emit(OpCodes.Ldc_I4, ps.Length)
        il.Emit(OpCodes.Newarr, typeof<obj>)
        il.Emit(OpCodes.Stloc,localArray)
        // Store arguments
        for argIndex = 0 to ps.Length - 1 do
            il.Emit(OpCodes.Ldloc, localArray)
            il.Emit(OpCodes.Ldc_I4, argIndex)
            il.Emit(OpCodes.Ldarg, argIndex + 1)
            let t = ps.[argIndex].ParameterType
            if not t.IsByRef then il.Emit(OpCodes.Box, t)
            il.Emit(OpCodes.Stelem_Ref)
        il.Emit(OpCodes.Call, typeof<MethodBase>.GetMethod("GetCurrentMethod"))
        il.Emit(OpCodes.Ldloc, localArray)
        il.Emit(OpCodes.Newobj, typeof<Invocation>.GetConstructor([|typeof<MethodBase>;typeof<obj[]>|]))

    /// Generate lock
    let generateLock (il:ILGenerator) emitFun =
        il.Emit(OpCodes.Ldarg_0)
        let mi = typeof<System.Threading.Monitor>.GetMethod("Enter", [|typeof<obj>|])
        il.Emit(OpCodes.Call, mi)
        emitFun ()
        il.Emit(OpCodes.Ldarg_0)
        let mi = typeof<System.Threading.Monitor>.GetMethod("Exit", [|typeof<obj>|])
        il.Emit(OpCodes.Call, mi)

    /// Generates invocation add
    let generateAddInvocation
        (il:ILGenerator) (invocationsField:FieldBuilder) =
        // Cons
        let cons () =
            il.Emit(OpCodes.Ldarg_1)
            il.Emit(OpCodes.Ldarg_0)
            il.Emit(OpCodes.Ldfld, invocationsField)
            let mi = typeof<Invocations>.GetMethod("Cons")
            il.Emit(OpCodes.Call, mi)
        il.Emit(OpCodes.Ldarg_0)
        cons ()
        il.Emit(OpCodes.Stfld, invocationsField)

    /// Generates trigger
    let generateTrigger (il:ILGenerator) (invokedField:FieldBuilder) =
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldfld, invokedField)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Newobj, typeof<EventArgs>.GetConstructor([||]))
        let trigger = typeof<Event<EventHandler,EventArgs>>.GetMethod("Trigger")
        il.Emit(OpCodes.Callvirt, trigger)

    /// Generates method overload
    let generateOverload 
        (il:ILGenerator)
        (argsLookup:ResizeArray<Value[]>,argsField:FieldBuilder)
        (returnValues:ResizeArray<Value>,returnValuesField:FieldBuilder) 
        (mi:MethodInfo,(args, result)) =
        /// Label to goto if argument fails
        let unmatched = il.DefineLabel()
        generateArgs il (argsLookup,argsField) (mi,args) unmatched
        generateReturn il (returnValues,returnValuesField) (mi,result)
        il.MarkLabel(unmatched)

    /// Module builder
    let moduleBuilder = lazy (
        let name = "Persimmock.Dynamic"
        /// Builder for assembly
        let assemblyBuilder =
            AssemblyBuilder.DefineDynamicAssembly(AssemblyName(name),AssemblyBuilderAccess.Run)
        /// Builder for module
        assemblyBuilder.DefineDynamicModule(name+".dll")
        )

    /// Defines a type builder for the specified abstract type
    let defineType (abstractType:Type) (others:Type list) =
        /// Stub name for abstract type
        let mockName = "Mock." + abstractType.Name.Replace("'", "!") + Guid.NewGuid().ToString()
        let parent, interfaces =
            if abstractType.IsInterface
            then typeof<obj>, [|yield abstractType;yield! others|]
            else abstractType, [||]
        let attributes = TypeAttributes.Public ||| TypeAttributes.Class
        let interfaces = [|yield typeof<IMockObject>; yield typeof<IMockRecorder>; yield! interfaces|]
        moduleBuilder.Value.DefineType(mockName, attributes, parent, interfaces)

    /// Builds a mock from the specified calls
    let mock (mode, abstractType:Type, otherTypes:Type list, 
              calls:(MethodInfo * (Arg[] * Result)) list, args:obj[], returnStrategy) =
        let isStrict = mode = MockMode.Strict
        /// Discover other interfaces
        let others =
            otherTypes 
            |> Seq.distinct
            |> Seq.filter ((<>) abstractType)
            |> Seq.filter (fun t -> abstractType.GetInterfaces() |> Seq.exists ((=) t) |> not)
            |> Seq.toList
        /// Builder for abstract type
        let typeBuilder = defineType abstractType others
        /// Field settings
        let fields = FieldAttributes.Private ||| FieldAttributes.InitOnly 
        /// Field for method return values
        let returnValuesField = typeBuilder.DefineField("_returnValues", typeof<obj[]>, fields)
        /// Field for method arguments 
        let argsField = typeBuilder.DefineField("_args", typeof<obj[][]>, fields)
        /// Field for method invocations
        let invocationsField = typeBuilder.DefineField("_invocations", typeof<Invocations>, fields)
        /// Field for invoked event
        let invokedField = typeBuilder.DefineField("_invoked", typeof<Event<EventHandler,EventArgs>>, fields)
        /// Field for verifiers
        let verifiersField = typeBuilder.DefineField("_verifiers", typeof<Verifiers>, fields)
        /// Return strategy field
        let returnField = typeBuilder.DefineField("_returnStrategy", typeof<Type->unit>, fields)
        // Generate default constructor
        generateConstructor typeBuilder [||] (fun il -> ())
        // Generates constructor body
        let generateConstructorBody (il:ILGenerator) =
            /// Constructor argument types
            let argTypes = [|for arg in args -> arg.GetType()|]
            // Call base constructor
            if args.Length = 0 then
                il.Emit(OpCodes.Ldarg_0)
                il.Emit(OpCodes.Call, typeof<obj>.GetConstructor(Type.EmptyTypes))
            else
                il.Emit(OpCodes.Ldarg_0)
                let bindings = 
                    BindingFlags.FlattenHierarchy ||| BindingFlags.Instance ||| 
                    BindingFlags.Public ||| BindingFlags.NonPublic 
                let ci = abstractType.GetConstructor(bindings, Type.DefaultBinder, argTypes, [||])
                argTypes |> Array.iteri (fun i arg ->
                    il.Emit(OpCodes.Ldarg, 4) 
                    il.Emit(OpCodes.Ldc_I4, i) 
                    il.Emit(OpCodes.Ldelem_Ref)
                    il.Emit(OpCodes.Unbox_Any, arg)
                )
                il.Emit(OpCodes.Call, ci)
            // Set fields
            il.Emit(OpCodes.Ldarg_0)
            il.Emit(OpCodes.Ldarg_1)
            il.Emit(OpCodes.Stfld, returnValuesField)
            il.Emit(OpCodes.Ldarg_0)
            il.Emit(OpCodes.Ldarg_2)
            il.Emit(OpCodes.Stfld, argsField)
            il.Emit(OpCodes.Ldarg_0)
            il.Emit(OpCodes.Ldarg_3)
            il.Emit(OpCodes.Stfld, returnField)
            il.Emit(OpCodes.Ldarg_0)
            let mi = typeof<Invocations>.GetMethod("get_Empty")
            il.Emit(OpCodes.Call, mi)
            il.Emit(OpCodes.Stfld, invocationsField)
            il.Emit(OpCodes.Ldarg_0)
            il.Emit(OpCodes.Newobj, typeof<Event<EventHandler,EventArgs>>.GetConstructor([||]))
            il.Emit(OpCodes.Stfld, invokedField)
            il.Emit(OpCodes.Ldarg_0)
            il.Emit(OpCodes.Newobj, typeof<Verifiers>.GetConstructor([||]))
            il.Emit(OpCodes.Stfld, verifiersField)
        // Generate constructor overload
        let constructorArgs = [|typeof<obj[]>;typeof<obj[][]>;typeof<Type->obj>;typeof<obj[]>|]
        generateConstructor typeBuilder constructorArgs generateConstructorBody
        // Generate Add method
        let mi = typeof<IMockRecorder>.GetMethod("Add")
        let addInvocation = defineMethod typeBuilder mi
        let il = addInvocation.GetILGenerator()
        generateLock il (fun () -> generateAddInvocation il invocationsField)
        il.Emit(OpCodes.Ret)
        // Generate Reset method
        let mi = typeof<IMockRecorder>.GetMethod("Reset")
        let reset = defineMethod typeBuilder mi
        let il = reset.GetILGenerator()
        let setEmpty () =
            il.Emit(OpCodes.Ldarg_0)
            let mi = typeof<Invocations>.GetMethod("get_Empty")
            il.Emit(OpCodes.Call, mi)
            il.Emit(OpCodes.Stfld, invocationsField)
        generateLock il setEmpty
        il.Emit(OpCodes.Ret)
        /// Generates a property getter
        let generatePropertyGetter name (field:FieldBuilder) =
            let mi = (typeof<IMockObject>.GetProperty(name).GetGetMethod())
            let getter = defineMethod typeBuilder mi
            let il = getter.GetILGenerator()
            il.Emit(OpCodes.Ldarg_0)
            il.Emit(OpCodes.Ldfld, field)
            il.Emit(OpCodes.Ret)
        // Generate IMockObject.Invocations property getter
        generatePropertyGetter "Invocations" invocationsField
        // Generate IMockObject.Verifiers property getter
        generatePropertyGetter "Verifiers" verifiersField
        /// Generates invoked event
        let generateEventHandler mi  handlerName =
            let add = defineMethod typeBuilder mi
            let il = add.GetILGenerator()
            il.Emit(OpCodes.Ldarg_0)
            il.Emit(OpCodes.Ldfld, invokedField)
            let get_Publish = typeof<Event<EventHandler,EventArgs>>.GetProperty("Publish").GetGetMethod()
            il.Emit(OpCodes.Callvirt, get_Publish)
            il.Emit(OpCodes.Ldarg_1)
            let handler = typeof<IDelegateEvent<EventHandler>>.GetMethod(handlerName)
            il.Emit(OpCodes.Callvirt, handler)
            il.Emit(OpCodes.Ret)
        // Generate IMockObject.Invoked event
        let invoked = typeof<IMockObject>.GetEvent("Invoked")
        generateEventHandler (invoked.GetAddMethod()) "AddHandler"
        generateEventHandler (invoked.GetRemoveMethod()) "RemoveHandler"
        /// Promotes generic methods to their generic method definition
        let definition (m:MethodInfo) = if m.IsGenericMethod then m.GetGenericMethodDefinition() else m
        /// Method overloads grouped by type
        let groupedMethods = calls |> Seq.groupBy (fst >> definition) |> Seq.toArray
        /// Method argument lookup
        let argsLookup = ResizeArray<obj[]>()
        /// Method return values
        let returnValues = ResizeArray<obj>()
        /// Structural method comparison
        let structurallyEqual (abstractMethod:MethodInfo) (mi:MethodInfo) =
            mi.Name = abstractMethod.Name &&
            mi.ReturnType = abstractMethod.ReturnType &&
            mi.GetParameters().Length = abstractMethod.GetParameters().Length &&
            Array.zip (mi.GetParameters()) (abstractMethod.GetParameters()) 
            |> Array.forall(fun (a,b) -> a.Attributes = b.Attributes && a.ParameterType = b.ParameterType)
        /// Method matches abstract method
        let matches (abstractMethod:MethodInfo) (mi:MethodInfo) = mi = abstractMethod || structurallyEqual abstractMethod mi
        /// Abstract type's methods including interfaces
        let abstractMethods = [|
            let attr = BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Instance
            let allMethods = abstractType.GetMethods(attr)
            let hasMethod mi = allMethods |> Seq.exists (matches mi)
            let isEqualsMethod (mi : MethodInfo) = mi.Name = "Equals" && (let ps = mi.GetParameters() in ps.Length = 1 && ps.[0].ParameterType = typeof<obj>)
            yield! allMethods |> Seq.filter (fun mi -> not mi.IsFinal && not (isEqualsMethod mi))
            let interfaces = abstractType.GetInterfaces()
            for interfaceType in interfaces do
                yield! interfaceType.GetMethods() |> Seq.filter (not << hasMethod)
            for interfaceType in others do //|> Seq.filter (fun t -> interfaces |> Seq.exists ((=) t) |> not) do
                yield! interfaceType.GetMethods() |> Seq.filter (not << hasMethod)
                for interfaceType in interfaceType.GetInterfaces() do
                    yield! interfaceType.GetMethods() |> Seq.filter (not << hasMethod)
            |]
        /// Generates a default value
        let generateDefaultValueReturn (il:ILGenerator) (returnType:Type) =
            let x = il.DeclareLocal(returnType)
            il.Emit(OpCodes.Ldloca_S, x.LocalIndex)
            il.Emit(OpCodes.Initobj, returnType)
            il.Emit(OpCodes.Ldloc, x.LocalIndex)
            il.Emit(OpCodes.Ret)
        /// Generates a call to returnStrategy
        let generateReturnStrategyCall (il:ILGenerator) (returnType:Type) =
            il.Emit(OpCodes.Ldarg_0)
            il.Emit(OpCodes.Ldfld, returnField)
            il.Emit(OpCodes.Ldtoken, returnType)
            let t = typeof<Type>.GetMethod("GetTypeFromHandle", [|typeof<RuntimeTypeHandle>|])
            il.Emit(OpCodes.Call, t)
            let invoke = FSharpType.MakeFunctionType(typeof<Type>,typeof<obj>).GetMethod("Invoke")
            il.Emit(OpCodes.Callvirt, invoke)
            il.Emit(OpCodes.Unbox_Any, returnType)
            il.Emit(OpCodes.Ret)
        // Implement abstract type's methods
        for abstractMethod in abstractMethods do
            /// Method builder
            let methodBuilder = defineMethod typeBuilder abstractMethod
            /// IL generator
            let il = methodBuilder.GetILGenerator()
            // Add invocation
            il.Emit(OpCodes.Ldarg_0)
            generateNewInvocation il invocationsField abstractMethod
            il.Emit(OpCodes.Callvirt, addInvocation)
            // Trigger invoked event
            generateTrigger il invokedField
            /// Method overloads defined for current method
            let overloads = groupedMethods |> Seq.tryFind (fst >> definition >> matches abstractMethod)
            match overloads with
            | Some (_, overloads) ->
                let toOverload = generateOverload il (argsLookup,argsField) (returnValues,returnValuesField)
                overloads |> Seq.toList |> List.rev |> Seq.iter toOverload
            | None -> ()
            if abstractMethod.ReturnType = typeof<System.Void> || abstractMethod.ReturnType = typeof<unit>
            then il.Emit(OpCodes.Ret)
            elif isStrict 
            then il.ThrowException(typeof<NotImplementedException>)
            else
                match returnStrategy with
                | Some _ -> generateReturnStrategyCall il abstractMethod.ReturnType
                | None -> 
                    let t = abstractMethod.ReturnType
                    if FSharpType.IsRecord t || FSharpType.IsTuple t || FSharpType.IsUnion t
                    then il.ThrowException(typeof<NotImplementedException>)
                    else generateDefaultValueReturn il abstractMethod.ReturnType
            if abstractType.IsInterface then
                typeBuilder.DefineMethodOverride(methodBuilder, abstractMethod)
        /// Setup return strategy
        let returnStrategy : Type -> obj = 
            match returnStrategy with Some f -> f | None -> fun t -> invalidOp "Expecting return strategy"
        /// Mock type
        let mockType = typeBuilder.CreateTypeInfo()
        // Generate object instance
        let args = [|box (returnValues.ToArray());box (argsLookup.ToArray()); box (returnStrategy);box args;|]
        Activator.CreateInstance(mockType, args)

open Emit
open Microsoft.FSharp.Quotations.Patterns

module internal QuotationEvaluation =
    /// Evaluates specified quotation
    let rec eval (env:(string * obj) list) = function
        | Value(v,t) -> v
        | Var(x) -> (env |> List.find (fst >> (=) x.Name)) |> snd
        | Coerce(e,t) -> eval env e
        | NewObject(ci,args) -> ci.Invoke(evalAll env args)
        | NewArray(t,args) ->
            let array = Array.CreateInstance(t, args.Length) 
            args |> List.iteri (fun i arg -> array.SetValue(eval env arg, i))
            box array
        | NewUnionCase(case,args) -> FSharpValue.MakeUnion(case, evalAll env args)
        | NewRecord(t,args) -> FSharpValue.MakeRecord(t, evalAll env args)
        | NewTuple(args) ->
            let t = FSharpType.MakeTupleType [|for arg in args -> arg.Type|]
            FSharpValue.MakeTuple(evalAll env args, t)
        | TupleGet(tuple, index) -> FSharpValue.GetTupleField(eval env tuple, index)
        | FieldGet(None,fi) -> fi.GetValue(null)
        | FieldGet(Some(x),fi) -> fi.GetValue(eval env x)
        | PropertyGet(None, pi, args) -> pi.GetValue(null, evalAll env args)
        | PropertyGet(Some(x),pi,args) -> pi.GetValue(eval env x, evalAll env args)
        | Call(None,mi,args) -> mi.Invoke(null, evalAll env args)
        | Call(Some(x),mi,args) -> mi.Invoke(eval env x, evalAll env args)
        | Lambda(var,body) ->
            let ft = FSharpType.MakeFunctionType(var.Type, body.Type)
            FSharpValue.MakeFunction(ft, fun arg -> eval ((var.Name,arg)::env) body)
        | Application(lambda, arg) ->
            let lambda = eval env lambda
            let flags = BindingFlags.Instance ||| BindingFlags.Public
            let mi = lambda.GetType().GetMethod("Invoke", flags, null, [|arg.Type|], null)
            let arg = eval env arg
            mi.Invoke(lambda, [|arg|])
        | Let(var, assignment, body) ->
            let env = (var.Name, eval env assignment)::env
            eval env body
        | Sequential(lhs,rhs) -> eval env lhs |> ignore; eval env rhs
        | IfThenElse(condition, t, f) ->
            if eval env condition |> unbox then eval env t else eval env f
        | UnionCaseTest(t,info) -> 
            let target = eval env t
            let case, _ = FSharpValue.GetUnionFields(target, info.DeclaringType)
            case.Tag = info.Tag |> box
        | TypeTest(v,t) -> t.IsAssignableFrom((eval env v).GetType()) |> box
        | DefaultValue(t) -> 
            match t.IsValueType with
            | true -> Activator.CreateInstance(t)
            | false -> Convert.ChangeType(null, t)
        | arg -> raise <| NotSupportedException(sprintf "Unsupported expression: %A" arg)
    and evalAll env args = [|for arg in args -> eval env arg|]

    let unwrapLambda =
        function
        | WithValue(_, _, Lambda(_, e))
        | Lambda(_, e) -> e
        | e -> raise <| NotSupportedException(sprintf "Expected a lambda expression, got: %A" e)

module Eval =
    open QuotationEvaluation
    let eval expr = eval [] expr

module internal Reflection =
    open Eval
    /// Returns true if method has specified attribute
    let hasAttribute a (mi:MethodInfo) = mi.GetCustomAttributes(a, true).Length > 0
    /// Matches attributed calls
    let (|AttributedArg|_|) = function
        | Call(_, mi, _) when hasAttribute typeof<WildcardAttribute> mi -> Some Any
        | Call(_, mi, [pred]) when hasAttribute typeof<PredicateAttribute> mi -> Some (Pred(eval pred))
        | _ -> None
    /// Converts parameter to arguments
    let rec toArg (pi:ParameterInfo,arg) =
        match pi, arg with
        | _, AttributedArg arg -> arg
        | pi, NewArray(_,args) when pi <> null && pi.GetCustomAttributes(typeof<ParamArrayAttribute>, true).Length > 0 ->
           ArgArray [|for arg in args -> toArg(null, arg)|]
        | _, expr -> eval expr |> Arg
    /// Converts parameters to arguments
    let toArgs ps args = [|for arg in Seq.zip ps args -> toArg arg |]
    /// Active pattern matches method call expressions
    let (|MethodCall|_|) expr =
        let areEqual args vars =
            let eq = function Var(arg),var -> arg = var | _ -> false
            vars |> List.rev |> List.zip args |> List.forall eq
        let rec traverse vars = function
            | Let(var,TupleGet(_,n),e) when n = List.length vars -> traverse (var::vars) e
            | Call(Some(x),mi,args) when vars.Length = args.Length && areEqual args vars -> 
                Some(x,mi,[|for arg in args -> Any|])
            | _ -> None
        traverse [] expr
    /// Converts member invocation expression to call information
    let toCall quote = 
        /// Unwrap applications of functions
        let unwrapApplication =
            /// Convert expression to arguments
            let toArgs' = 
                List.map (fun expr ->
                    match expr with
                    | AttributedArg arg -> arg
                    | _ -> eval expr |> Arg
                )
                >> List.toArray
            /// Unwrap quotation accumulating argument values
            let rec unwrap values quote = 
                match quote with
                | Application (inner, value) -> unwrap (value :: values) inner  // Normal application
                | Lambda (_, body) -> unwrap values body
                | Call (Some expr, info, _) -> (expr, info, toArgs' values) 
                | _ -> raise <| NotSupportedException(sprintf "Expected function application: %A" quote)
            unwrap []
        /// Unwrap standard quotation of member value
        let rec unwrapStandard quote = 
            match quote with
            | Lambda(unitVar,Call(Some(x),mi,[])) -> x, mi, [||]
            | Lambda(unitVar,Call(Some(x),mi,[NewArray(_)])) -> x, mi, [|Any|]
            | Lambda(a,Call(Some(x),mi,[Var(a')])) when a=a' -> x, mi, [|Any|]
            | Lambda(_,MethodCall(x,mi,args)) -> x, mi, args
            | Call(Some(x), mi, args) ->
                x, mi, toArgs (mi.GetParameters()) args
            | Call(Some(ValueWithName(x, _, _)), mi, args) ->
                x :?> Quotations.Expr, mi, toArgs (mi.GetParameters()) args
            | PropertyGet(Some(x), pi, args) -> 
                x, pi.GetGetMethod(), toArgs (pi.GetIndexParameters()) args
            | PropertySet(Some(x), pi, args, value) -> 
                x, pi.GetSetMethod(), toArgs [yield! pi.GetIndexParameters();yield null] [yield! args;yield value]
            | ValueWithName(x, _, _) ->
                unwrapStandard (x :?> Quotations.Expr)
            | expr -> raise <| NotSupportedException(sprintf "Expected standard function application: %A" expr)
        // Handle applications of functions and standard functions
        match quote with
        | Application (_, _) -> unwrapApplication quote
        | _ -> unwrapStandard quote
    /// Converts expression to call checking expected type
    let toCallOf abstractType expr =
        match toCall expr with
        | x, mi, args when x.Type = abstractType -> mi, args
        | x, _, _ -> raise <| NotSupportedException(sprintf "Expected call on abstract type %A, got %A" abstractType x.Type)
    /// Converts Mock.With expressions to calls with expected results list
    let rec toCallResult = function
        | ForIntegerRangeLoop(v,a,b,y) -> [for i = eval a :?> int to eval b :?> int do yield! toCallResult y]
        | Sequential(x,y) -> toCallResult x @ toCallResult y
        | Call(None, mi, [lhs;rhs]) when hasAttribute typeof<ReturnsAttribute> mi -> 
            let x, mi, args = toCall lhs
            let returns = ReturnValue(eval rhs, mi.ReturnType)
            [x, mi,(args,returns)]
        | Call(None, mi, [lhs;rhs]) when hasAttribute typeof<CallsAttribute> mi -> 
            let x, mi, args = toCall lhs
            let returns = ReturnFunc(eval rhs, mi.ReturnType)
            [x, mi,(args,returns)]
        | Call(None, mi, [lhs;rhs]) when hasAttribute typeof<RaisesAttribute> mi -> 
            let x, mi, args = toCall lhs
            let raises = RaiseValue(eval rhs :?> exn)
            [x, mi,(args,raises)]
        | Call(Some(x), mi, args) when mi.ReturnType = typeof<unit> || mi.ReturnType = typeof<Void> ->
            [x, mi,(toArgs (mi.GetParameters()) args,Unit)]
        | PropertySet(Some(x), pi, args, value) ->
            [x, pi.GetSetMethod(),(toArgs [|yield! pi.GetIndexParameters(); yield null|] [yield! args; yield value], Unit)]
        | expr -> invalidOp(expr.ToString())
    /// Converts Mock.With expressions to call/result list checking expected type
    let rec toCallResultOf abstractType expr =
        let calls = toCallResult expr
        [for (x,mi,(arg,result)) in calls -> 
            if x.Type = abstractType
            then mi,(arg,result)
            else raise <| NotSupportedException(sprintf "Expected call on abstract type %A, got %A" abstractType x.Type)]
    /// Converts expression to corresponding event Add and Remove handlers
    let toHandlers abstractType = function
        | Call(None, mi, [Lambda(_,Call(Some(x),addHandler,_));
                          Lambda(_,Call(Some(_),removeHandler,_));_]) ->
            if x.Type = abstractType
            then addHandler, removeHandler
            else raise <| NotSupportedException(sprintf "Expected call on abstract type %A, got %A" abstractType x.Type)
        | expr -> raise <| NotSupportedException(sprintf "Expected call expression: %A" expr)
