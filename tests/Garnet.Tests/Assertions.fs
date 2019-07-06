namespace Garnet.Tests

open Expecto

[<AutoOpen>]
module Assertions =
    let shouldEqual b a =
        Expect.equal a b ""

    let shouldNotEqual b a =
        Expect.notEqual a b ""
          