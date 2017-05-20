module Persimmock.Tests.VerifyTest

open System.Collections.Generic
open Persimmock
open Persimmon
open UseTestNameByReflection

let ``expect method call with any argument value`` = test {
  let xs = mock<IList<int>> {
    method (fun xs -> xs.Contains)
    args (any<int>)
    returns (fun () -> true)
  }
  Mock.Expect(<@ xs.Contains(0) @>, never)
  Mock.Expect(<@ xs.Contains(any<int>) @>, once)
  xs.Contains(1) |> ignore
  return Mock.VerifyAll(xs)
}

let ``expect method is called the specified number of times`` = test {
  let xs = mock<IList<int>> { () }
  Mock.Expect(<@ xs.Contains(0) @>, never)
  Mock.Expect(<@ xs.Contains(1) @>, once)
  Mock.Expect(<@ xs.Contains(2) @>, exactly 2)
  xs.Contains(1) |> ignore
  xs.Contains(2) |> ignore
  xs.Contains(2) |> ignore
  return Mock.VerifyAll(xs)
}

let ``verify method call with any argument value`` = test {
  let xs = mock<IList<int>> {
    method (fun xs -> xs.Contains)
    args (any<int>)
    returns (fun () -> true)
  }
  xs.Contains(1) |> ignore
  Mock.Verify(<@ xs.Contains(0) @>, never)
  Mock.Verify(<@ xs.Contains(any<int>) @>, once)
  return ()
}

let ``verify method call with specific argument value`` = test {
  let xs = mock<IList<int>> {
    method (fun xs -> xs.Contains)
    args (any<int>)
    returns (fun () -> true)
  }
  Mock.Verify(<@ xs.Contains(1) @>, never)
  xs.Contains(0) |> ignore
  Mock.Verify(<@ xs.Contains(1) @>, never)
  xs.Contains(1) |> ignore
  Mock.Verify(<@ xs.Contains(1) @>, once)
  return ()
}

let ``verify method call with matching argument value`` = test {
  let xs = mock<IList<int>> {
    method (fun xs -> xs.Contains)
    args (any<int>)
    returns (fun () -> true)
  }
  Mock.Verify(<@ xs.Contains(is (fun x -> x > 0)) @>, never)
  xs.Contains(1) |> ignore
  Mock.Verify(<@ xs.Contains(is (fun x -> x > 0)) @>, once)
  return ()
}

let ``expect property getter`` = test {
  let xs = mock<IList<int>> {
    property (fun xs -> xs.Count)
    returns (fun () -> 1)
  }
  Mock.Expect(<@ xs.Count @>, once)
  xs.Count |> ignore
  return Mock.VerifyAll(xs)
}

let ``verify property getter`` = test {
  let xs = mock<IList<int>> { () }
  Mock.Verify(<@ xs.Count @>, never)
  xs.Count |> ignore
  return Mock.Verify(<@ xs.Count @>, once)
}

let ``expect action`` = test {
  let xs = mock<IList<int>> { () }
  expect <@ xs.Clear() @> once
  xs.Clear()
  return Mock.VerifyAll(xs)
}

let ``verify action`` = test {
  let xs = mock<IList<int>> { () }
  Mock.Verify(<@ xs.Clear() @>, never)
  xs.Clear()
  return Mock.Verify(<@ xs.Clear() @>, once)
}

let ``expect property setter`` = test {
  let xs = mock<IList<int>> { () }
  Mock.Expect(<@ xs.[0] <- 1 @>, once)
  xs.[0] <- 1
  return Mock.VerifyAll(xs)
}

let ``verify property setter`` = test {
  let xs = mock<IList<int>> { () }
  Mock.Verify(<@ xs.[0] <- 1 @>, never)
  xs.[0] <- 1
  return Mock.Verify(<@ xs.[0] <- 1 @>, once)
}

let ``verify sequence``  = test {
  let xs = mock<IList<int>> { () }
  
  xs.Clear()
  let value = xs.[0]
  xs.[0] <- value
  xs.Count |> ignore
  xs.Contains(1) |> ignore

  return Mock.VerifySequence
    <@
      xs.Clear()
      xs.[any<int>] --> any<int>
      xs.[any<int>] <- any<int>
      xs.Count --> any<int>
      xs.Contains(any<int>) --> any<bool>
    @>
}

let ``verify repeat`` =
  let xs = mock<IList<int>> { () }
  for i = 1 to 10 do xs.Add(any<int>)
  test {
    return Mock.VerifySequence
      <@
        for i = 1 to 10 do xs.Add(any<int>)
      @>
  }

let ``verify repeat sequence`` =
  let xs = mock<IList<int>> { () }
  xs.Clear()
  for i = 1 to 10 do 
    xs.Add(i)
    xs.RemoveAt(0)
  xs.Count |> ignore
  test {
    return Mock.VerifySequence
      <@
        xs.Clear()
        for i = 1 to 10 do 
          xs.Add(any<int>)
          xs.RemoveAt(0)
        xs.Count --> any<int>
      @>
}

type IFoo =
  abstract Bar<'T> : 'T -> bool

let ``verify generic method`` = test {
  let foo = mock<IFoo> {
    method (fun x -> x.Bar)
    args 1
    returns (fun () -> true)
  }
  foo.Bar(1) |> ignore
  return verify <@ foo.Bar(1) @> once
}

type IFoo<'T> =
  abstract Bar : 'T -> bool

let ``verify generic interface's method`` = test {
  let foo = mock<IFoo<int>> {
    method (fun x -> x.Bar)
    args 1
    returns (fun () -> true)
  }
  foo.Bar(1) |> ignore
  return verify <@ foo.Bar(1) @> once
}

let ``verification failure exception messages are informative (no calls)`` = test {
  let xs = mock<IList<int>> { () }

  let! ex = trap { it (Mock.Verify (<@ xs.IndexOf(1) @>, exactly 2)) }
  do!
    ex.Message
    |> assertEquals "Expected exactly (=) 2 calls that match the expected pattern, but saw 0.\nExpected: IndexOf(1)"
}

let ``verification failure exception messages are informative (one call)`` = test {
  let xs = mock<IList<int>> { () }
  xs.IndexOf(90) |> ignore
  xs.IndexOf(25) |> ignore

  let! ex = trap { it (Mock.Verify (<@ xs.IndexOf(90) @>, exactly 2)) }
  do!
    ex.Message
    |> assertEquals "Expected exactly (=) 2 calls that match the expected pattern, but saw 1.\nExpected: IndexOf(90)\nActual:\n1. IndexOf(90)\n2. IndexOf(25)"
}

type IArrayConsumer =
  abstract ConsumeArray : int[] -> bool

let ``structural equality is used for array arguments`` = test {
  let xs = mock<IArrayConsumer> { () }
  xs.ConsumeArray([|15|]) |> ignore
  return Mock.Verify <@ xs.ConsumeArray([|15|]) @>
}
