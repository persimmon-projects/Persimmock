module Persimmock.Tests.GenericsTest

open System
open System.ComponentModel
open System.Collections.Generic
open Persimmock
open Persimmon
open UseTestNameByReflection

type IInterface10 =
  abstract Arity1<'a> : 'a -> bool

let ``one generic argument`` = test {
  do!
    (mock<IInterface10> {
      method(fun mock -> mock.Arity1)
      args (It.IsAny ())
      returns(fun () -> true)
    }).Arity1(It.IsAny ())
    |> assertPred
}

let ``one generic argument with value specified`` = test {
  do!
    (mock<IInterface10> {
      method(fun mock -> mock.Arity1)
      args 1
      returns(fun () -> true)
    }).Arity1(1)
    |> assertPred
}

type IInterface11 =
  abstract Arity2<'a> : 'a * int -> bool

let ``one generic argument & one non-generic`` = test {
  do!
    (mock<IInterface11> {
      method(fun mock -> mock.Arity2)
      args (It.IsAny (), It.IsAny ())
      returns(fun () -> true)
    }).Arity2(It.IsAny (), It.IsAny ())
    |> assertPred
}

let ``one generic argument one & one int value`` = test {
  do!
    (mock<IInterface11> {
      method(fun mock -> mock.Arity2)
      args (It.IsAny (), 1)
      returns(fun () -> true)
    }).Arity2(It.IsAny (), 1)
    |> assertPred
}

type IInterface21 =
    abstract Arity3<'a,'b> : 'a * 'b * int -> bool

let ``two generic arguments & one non-generic`` = test {
  do!
    (mock<IInterface21> {
      method(fun mock -> mock.Arity3)
      args (It.IsAny (), It.IsAny(), It.IsAny ())
      returns(fun () -> true)
    }).Arity3(It.IsAny (), It.IsAny (), It.IsAny ())
    |> assertPred
}

type IInterface<'a> =
  abstract Arity1 : 'a -> bool

let ``generic interface`` = test {
    do!
    (mock<IInterface<_>> {
      method(fun mock -> mock.Arity1)
      args (It.IsAny ())
      returns(fun () -> true)
    }).Arity1(It.IsAny ())
    |> assertPred
}

type IInterface' =
  abstract Arity0<'a> : unit -> 'a

let ``generic return value`` = test {
  let m = mock<IInterface'> {
    //setup (fun mock -> mock.Arity0())
    //returns(fun () -> 1)
    method (fun mock -> mock.Arity0)
    call (fun () -> 1)
  }
  do! m.Arity0() |> assertEquals 1
}

type ISettings =
  abstract Get: key:string * fallback:'a -> 'a

let ``generic argument and return value`` = test {
  let m = mock<ISettings> {
    //setup (fun mock -> mock.Get(any<string>, It.IsAny()))
    //hook (fun (_, x: int) -> x)
    method (fun mock -> mock.Get)
    call (fun (_, x: int) -> x)
  }
  let expected = 1
  do! m.Get("Key", expected) |> assertEquals expected
}

let ``generic argument and return value setup over multiple members`` = test {
  let expected = 1.0
  let m = mock<ISettings> {
    // setup (fun mock -> mock.Get(any<string>, It.IsAny()))
    // returns (fun () -> 2)
    // setup (fun mock -> mock.Get(any<string>, It.IsAny()))
    // returns (fun () -> expected)
    method (fun mock -> mock.Get)
    call (fun (_, _: int) -> 2)
    method (fun mock -> mock.Get)
    call (fun (_, _: float) -> expected)
  }
  do! m.Get("Key", expected) |> assertEquals expected
}
