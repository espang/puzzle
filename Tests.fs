namespace Puzzle

open Xunit
open Xunit.Abstractions
open Board

type Tests(output:ITestOutputHelper) =

    [<Fact>]
    let add_5_to_3_should_be_8() =
        Assert.Equal(true, true)

