module FsYahtzee

open Xunit
open Xunit.Extensions
open FsUnit.Xunit

type Dice = One | Two | Three | Four | Five

type Category = Ones | Twos | Threes | Pair | ``Three of a kind``

let diceScore = function | One -> 1 | Two -> 2 | Three -> 3

let rec allAreEqual =
    function
    | f :: s :: rest when f <> s -> None
    | f :: s :: rest when f = s -> (s :: rest) |> allAreEqual
    | f :: [] -> Some f

let calculateOfAKind count throw =
    let possibleScore =
        List.sort >> List.rev >> Seq.windowed count
        >> Seq.map List.ofArray
        >> Seq.tryPick allAreEqual
        >> Option.map (diceScore >> ((*) count))
    defaultArg (possibleScore throw) 0

let calculateSingle throw category =
    let diceToCount =
        match category with
        | Ones -> One
        | Twos -> Two
        | Threes -> Three
    throw
    |> List.filter ((=) diceToCount)
    |> List.length
    |> (*) (diceScore diceToCount)

let score (dOne, dTwo, dThree, dFour, dFive) =
    let throw = [dOne; dTwo; dThree; dFour; dFive]
    function
    | Pair -> calculateOfAKind 2 throw
    | ``Three of a kind`` -> calculateOfAKind 3 throw
    | category -> calculateSingle throw category

[<Fact>]
let ``one placed in ones category`` ()= 
    let throw = One, One, One, One, One
    let category = Ones
    score throw category
    |> should equal 5

[<Fact>]
let ``two placed in ones category`` () =
    let throw =Two, Two, Two, Two, Two
    let category = Ones
    score throw category
    |> should equal 0

[<Fact>]
let ``One dice One placed in category Ones`` () =
    let throw =  One, Two, Two, Two, Two
    let category = Ones
    score throw category |> should equal 1

[<Fact>]
let ``two placed in twos category`` ()= 
    let throw = Two, Two, Two, Two, Two
    let category = Twos
    score throw category
    |> should equal 10

[<Fact>]
let ``two placed in threes category`` ()= 
    let throw = Two, Two, Two, Two, Two
    let category = Threes
    score throw category
    |> should equal 0

[<Fact>]
let ``three placed in threes category`` ()= 
    let throw = Three, Three, Three, Three, Three
    let category = Threes
    score throw category
    |> should equal 15

[<Fact>]
let ``pair of twos and rest ones placed in pair category`` ()= 
    let throw = One, One, One, Two, Two
    let category = Pair
    score throw category
    |> should equal 4

[<Fact>]
let ``one placed in pair category`` ()= 
    let throw = One, One, One, One, One
    let category = Pair
    score throw category
    |> should equal 2

[<Fact>]
let ``Straight in pair category`` ()= 
    let throw = One, Two, Three, Four, Five
    let category = Pair
    score throw category
    |> should equal 0

[<Fact>]
let ``pair of ones pair of twos and one three placed in Pair category`` () =
    let throw = Three, One, Two, One, Two
    score throw Pair |> should equal 4

[<Fact>]
let ``three of ones pair of twos placed in Triple category`` () =
    let throw = One, Two, One, Two, One
    score throw ``Three of a kind`` |> should equal 3