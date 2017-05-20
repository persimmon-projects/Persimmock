module Persimmock.Tests.MockTest

open System
open System.ComponentModel
open System.Collections.Generic
open Persimmock
open Persimmon
open UseTestNameByReflection

type IInterface =
  abstract MethodReturnsSomething : unit -> int
  abstract MethodReturnsNothing : unit -> unit
  abstract MethodReturnsOption: unit -> int option
  abstract Arity1Method : int -> bool
  abstract Arity1MethodReturnsNothing : int -> unit
  abstract Arity2Method : int * string -> bool
  abstract Arity2MethodReturnsNothing : int * string -> unit
  abstract Arity3Method : int * string * float -> bool
  abstract Arity3MethodReturnsNothing : int * string * float -> unit
  abstract StringProperty : string

let ``an interface method that is not implemented should return the default value`` = test {
  let stub = mock<IInterface> { () }
  do! stub.MethodReturnsSomething() |> assertEquals Unchecked.defaultof<int>
}

let ``an interface method that is not implemented and returns nothing should not throw`` = test {
  let stub = mock<IInterface> { () }
  return stub.MethodReturnsNothing()
}

let ``an implemented interface method should return a specified option value of None`` = test {
  let stub = mock<IInterface> {
    method (fun x -> x.MethodReturnsOption)
    call (fun () -> None)
  }
  do! stub.MethodReturnsOption() |> assertEquals None
}

let ``an implemented interface method should return a specified option value of Some`` = test {
  let stub = mock<IInterface> {
    method (fun x -> x.MethodReturnsOption)
    call (fun () -> Some 1)
  }
  do! stub.MethodReturnsOption() |> assertEquals (Some 1)
}

let ``an implemented interface property getter should return the specified value`` = test {
  let value = "Mock"
  let stub = mock<IInterface> {
    property (fun x -> x.StringProperty)
    returns (fun () -> value)
  }
  do! stub.StringProperty |> assertEquals value
}

let ``an implemented interface property getter should return the specified computed value`` = test {
  let counter = 1
  let stub = mock<System.Collections.IList> {
    property (fun x -> x.Count)
    returns (fun () -> counter)
  }
  do! stub.Count |> assertEquals counter
}

let ``an implemented interface method with arity/1 should accept any arguments`` = parameterize {
  source [-1; 0; 1]
  run (fun n -> test {
    let stub = mock<IInterface> {
      method (fun x -> x.Arity1Method)
      args (any<int>)
      returns (fun () -> true)
    }
    do! stub.Arity1Method(n) |> assertPred
  })
}

let ``an implemented interface method with arity/1 which returns nothing should not throw`` = parameterize {
  source [-1; 0; 1]
  run (fun n -> test {
    let stub = mock<IInterface> {
      method (fun x -> x.Arity1MethodReturnsNothing)
      args (any<int>)
      returnUnit
    }
    return stub.Arity1MethodReturnsNothing(n)
  })
}

let ``an implemented composite interface method with arity/1 should accept any arguments`` = parameterize {
  source [""; "NotEmpty"]
  run (fun x -> test {
    let stub = mock<System.Collections.IList> {
      method (fun x -> x.Contains)
      args (any<obj>)
      returns (fun () -> true)
    }
    do! stub.Contains(x) |> assertPred
  })
}

let ``an implemented interface method with arity/2 should accept any arguments`` = parameterize {
  source [-1; 0; 1]
  run (fun n -> test {
    let stub = mock<IInterface> {
      method (fun x -> x.Arity2Method)
      args (any<int>, any<string>)
      returns (fun () -> true)
    }
    do! stub.Arity2Method(n, "string") |> assertPred
  })
}

let ``an implemented interface method with arity/2 which returns nothing should not throw`` = parameterize {
  source [-1; 0; 1]
  run (fun n -> test {
    let stub = mock<IInterface> {
      method (fun x -> x.Arity2MethodReturnsNothing)
      args (any<int>, any<string>)
      returnUnit
    }
    return stub.Arity2MethodReturnsNothing(n, "string")
  })
}

let ``reference type arguments should accept and match null`` = test {
  let stub = mock<IInterface> {
    method (fun x -> x.Arity2Method)
    args (any<int>, null)
    returns (fun () -> true)
  }
  do! stub.Arity2Method(1, null) |> assertPred
}

let ``an implemented interface method with arity/2 should be callable`` = parameterize {
  source [
    -1, ""
    -1, "NotEmpty"
    0, ""
    0, "NotEmpty"
    1, ""
    1, "NotEmpty"
  ]
  run (fun (n, s) -> test {
    let stub = mock<IInterface> {
      method (fun x -> x.Arity2Method)
      call (fun (_, _) -> true)
    }
    do! stub.Arity2Method(n, s) |> assertPred
  })
}

let arity3 = [
  -1, "", Double.NegativeInfinity
  -1, "NotEmpty", Double.NegativeInfinity
  -1, "", 0.
  -1, "NotEmpty", 0.
  -1, "", Double.MaxValue
  -1, "NotEmpty", Double.MaxValue
  0, "", Double.NegativeInfinity
  0, "NotEmpty", Double.NegativeInfinity
  0, "", 0.
  0, "NotEmpty", 0.
  0, "", Double.MaxValue
  0, "NotEmpty", Double.MaxValue
  1, "", Double.NegativeInfinity
  1, "NotEmpty", Double.NegativeInfinity
  1, "", 0.
  1, "NotEmpty", 0.
  1, "", Double.MaxValue
  1, "NotEmpty", Double.MaxValue
]

let ``an implemented interface method with arity/3 should be callable`` = parameterize {
  source arity3
  run (fun (n, s, d) -> test {
    let stub = mock<IInterface> {
      method (fun x -> x.Arity3Method)
      call (fun (_, _, _) -> true)
    }
    do! stub.Arity3Method(n, s, d) |> assertPred
  })
}

let ``an implemented interface method with arity/3 should match specified arguments`` = parameterize {
  source arity3
  run (fun (n, s, d) -> test {
    let stub = mock<IInterface> {
      method (fun x -> x.Arity3Method)
      args (n, s, d)
      returns (fun () -> true)
    }
    do! stub.Arity3Method(n, s, d) |> assertPred
  })
}

let ``an implemented interface method with arity/3 should match specified argument predicates`` = parameterize {
  source arity3
  run (fun (n, s, d) -> test {
    let stub = mock<IInterface> {
      method (fun x -> x.Arity3Method)
      args (is ((=) n), is ((=) s), is ((=) d))
      returns (fun () -> true)
    }
    do! stub.Arity3Method(n, s, d) |> assertPred
  })
}

let ``an implemented interface method should match a predicate with named arguments`` = test {
  let instance = mock<System.Collections.Generic.IList<int>> {
    method (fun x -> x.Remove)
    args (is (fun i -> i >= 0))
    returns (fun () -> true)
  }
  do! instance.Remove(99) |> assertPred
}

let ``an implemented interface method with arity/3 should match correct method pattern`` = parameterize {
  source arity3
  run (fun (n, s, d) -> test {
    let stub = mock<IInterface> {
      method (fun x -> x.Arity3Method)
      args (is ((<>) n), is ((<>) s), is ((<>) d))
      returns (fun () -> false)
      method (fun x -> x.Arity3Method)
      args (any<int>, any<string>, any<float>)
      returns (fun () -> true)
    }
    do! stub.Arity3Method(n, s, d) |> assertPred
  })
}

let ``an implemented interface method can raise a specified exception value`` = test {
  let message = "Message"
  let stub = mock<IInterface> {
    method (fun x -> x.MethodReturnsNothing)
    raise (System.ApplicationException(message))
  }
  let! ex = trap { it (stub.MethodReturnsNothing()) }
  do! ex.GetType() |> assertEquals typeof<System.ApplicationException>
  do! ex.Message |> assertEquals message
}

let ``an implemented interface returning method can raise a specified exception value`` = test {
  let message = "Message"
  let stub = mock<IInterface> {
    method (fun x -> x.MethodReturnsSomething)
    raise (System.ApplicationException(message))
  }
  let! ex = trap { it (stub.MethodReturnsSomething()) }
  do! ex.GetType() |> assertEquals typeof<System.ApplicationException>
  do! ex.Message |> assertEquals message
}

let ``an implemented interface property can raise a specified exception value`` = test {
  let message = "Message"
  let stub = mock<IInterface> {
    property (fun x -> x.StringProperty)
    raise (System.ApplicationException(message))
  }
  let! ex = trap { it (stub.StringProperty) }
  do! ex.GetType() |> assertEquals typeof<System.ApplicationException>
  do! ex.Message |> assertEquals message
}

[<AbstractClass>]
type Shape2D(x0 : float, y0 : float) =
  let mutable x, y = x0, y0
  let mutable rotAngle = 0.0

  member __.CenterX with get() = x and set xval = x <- xval
  member __.CenterY with get() = y and set yval = y <- yval

  abstract Perimeter : float with get
  abstract Name : string with get

  member __.Move dx dy =
     x <- x + dx
     y <- y + dy

  abstract member Rotate: float -> unit
  default __.Rotate(angle) = rotAngle <- rotAngle + angle

let ``an implemented abstract class property should return the specified value`` = test {
  let stub = mock<Shape2D> {
    property (fun x -> x.Name)
    returns (fun () -> "Name")
  }
  do! assertEquals "Name" stub.Name
}

[<AbstractClass>]
type AbstractBaseClass() =
  abstract member Add: int * int -> int
  abstract member Pi: float
  abstract member Area: float with get,set

let ``an implemented abstract base class method should return the specified value`` = test {
  let stub = mock<AbstractBaseClass> {
    method (fun x -> x.Add)
    args (any<int>, any<int>)
    returns (fun () -> 2)
  }
  do! stub.Add(1, 1) |> assertEquals 2
}

let ``an implemented abstract base class property should return the specified value`` = test {
  let stub = mock<AbstractBaseClass> {
    property (fun x -> x.Pi)
    returns (fun () -> 4.0)
  }
  do! assertEquals 4.0 stub.Pi
}

let ``an implemented abstract base class property setter should accept the specified value`` = test {
  let specifiedValue = ref None
  let stub = mock<AbstractBaseClass> {
    property (fun x -> x.Area <- any<float>)
    hook (fun x -> specifiedValue := Some x)
  }
  let area = 16.0
  stub.Area <- area
  do! assertEquals (Some area) !specifiedValue
}

[<AbstractClass>]
type AbstractBaseClassWithConstructorArgs(version: int, name: string) =
  new (version: int) = AbstractBaseClassWithConstructorArgs(version, "")
  member __.Version = version
  member __.Name = name
  abstract AbstractMethod : unit -> int

let ``an abstract base class constructor should receive specified arguments`` = test {
  let m = mock<AbstractBaseClassWithConstructorArgs> {
    initialize 7
  }
  do! assertEquals 7 m.Version
  let m = mock<AbstractBaseClassWithConstructorArgs> {
    initialize (7, "seven")
  }
  do! assertEquals "seven" m.Name
}

type BaseClass() =
  abstract AbstractMethod : unit -> bool
  default __.AbstractMethod() = false
  member __.ConcreteMethod() = true
  abstract AbstractProperty: bool
  default __.AbstractProperty = false
  member __.ConcreteProperty = true

let ``an implemented base class method should return the specified value`` = test {
  let stub = mock<BaseClass> {
    method (fun x -> x.AbstractMethod)
    call (fun () -> true)
  }
  do! stub.AbstractMethod() |> assertPred
}

let ``an implemented base class property should return the specified value`` = test {
  let stub = mock<BaseClass> {
    property (fun x -> x.AbstractProperty)
    returns (fun () -> true)
  }
  do! assertPred stub.AbstractProperty
}

let ``an implemented interface event can add handlers`` = test {

  let ev = Event<_, _>()
  let instance = mock<INotifyPropertyChanged> {
    event (fun x -> x.PropertyChanged)
    publish (ev.Publish)
  }
  let triggered = ref false
  instance.PropertyChanged.Add(fun x -> triggered := true)
  ev.Trigger(instance, PropertyChangedEventArgs("X"))
  do! assertPred !triggered
}

let ``an implemented interface event can add/remove handlers`` = test {

  let ev = Event<_, _>()
  let instance = mock<INotifyPropertyChanged> {
    event (fun x -> x.PropertyChanged)
    publish (ev.Publish)
  }
  let triggered = ref false
  let setTriggered s e = triggered := true
  let handler = PropertyChangedEventHandler(setTriggered)
  instance.PropertyChanged.AddHandler(handler)
  instance.PropertyChanged.RemoveHandler(handler)
  ev.Trigger(instance, PropertyChangedEventArgs("X"))
  do! assertEquals false !triggered
}

let ``test strict mode`` = test {
  let m = mock<IList<int>> {
    mode (MockMode.Strict)
  }
  let! ex = trap { it (m.Count) }
  do! ex.GetType() |> assertEquals typeof<NotImplementedException>
}

let ``test loose mode`` = test {
  let m = mock<IList<int>> {
    mode (MockMode.Loose)
  }
  do! assertEquals Unchecked.defaultof<int> m.Count
}

let ``test unimplemented member with out arg`` = test {
  let m = mock<IDictionary<string,int>> { () }   
  let hasValue, value = m.TryGetValue("x")
  do! assertEquals false hasValue
}

let ``test return strategy is used on mock of`` = test {
  let m = mock<IInterface> {
    strategy (fun t -> box 1)
  }
  do! m.MethodReturnsSomething() |> assertEquals 1
}

let ``test a default return strategy can be used`` = test {
  let defaultStrategy (t:System.Type) =
    if t.IsValueType then System.Activator.CreateInstance(t)
    else null
  let m = mock<IInterface> {
    strategy defaultStrategy
  }
  do! m.Arity1Method(1) |> assertEquals false
  do! m.MethodReturnsOption() |> assertEquals None
}

let ``test return strategy is used on mock create`` = test {
  let m = Mock<IInterface>(returnStrategy = fun t -> box "String").Create()
  do! assertEquals "String" m.StringProperty
}
