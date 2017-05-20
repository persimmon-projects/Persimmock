[<AutoOpen>]
module Persimmock.Syntax

let mock<'TAbstract when 'TAbstract : not struct> = MockBuilder<'TAbstract>()

[<Wildcard; RequiresExplicitTypeArguments>]
let inline any<'TArg> : 'TArg = It.IsAny<'TArg>()

[<Predicate>]
let inline is (f: 'TArg -> bool) : 'TArg = It.Is(f)

[<Returns>]
let (-->) (source:'T) (value:'T) = ()

[<Calls>]
let (--->) (source:'T) (value:unit->'T) = ()

[<Raises>]
let (==>) (source:'T) (value:exn) = ()

let verify expr times = Mock.Verify(expr, times)
let expect expr times = Mock.Expect(expr, times)
let verifyAll mock = Mock.VerifyAll(mock)
let verifySeq expr = Mock.VerifySequence(expr)
