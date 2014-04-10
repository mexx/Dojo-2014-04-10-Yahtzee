module FsYahtzee

open Xunit
open Xunit.Extensions
open FsUnit.Xunit

type Dice = One | Two | Three

type Category = Ones | Twos | Threes | Pair

let diceScore = function | One -> 1 | Two -> 2 | Three -> 3

let calculatePair throw =
    throw
    |> List.sort
    |> List.rev
    |> List.head
    |> diceScore
    |> (*) 2

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
    | Pair -> calculatePair throw
    | category -> calculateSingle throw category

[<Fact>]
let ``Five dices with one placed in ones category`` ()= 
    let throw = Dice.One, Dice.One, Dice.One, Dice.One, Dice.One
    let category = Category.Ones
    score throw category
    |> should equal 5

[<Fact>]
let ``Five dices with two placed in ones category`` () =
    let throw =Dice.Two, Dice.Two, Dice.Two, Dice.Two, Dice.Two
    let category = Category.Ones
    score throw category
    |> should equal 0

[<Fact>]
let ``One dice One placed in category Ones`` () =
    let throw =  Dice.One, Dice.Two, Dice.Two, Dice.Two, Dice.Two
    let category = Category.Ones
    score throw category |> should equal 1

[<Fact>]
let ``Five dices with two placed in twos category`` ()= 
    let throw = Dice.Two, Dice.Two, Dice.Two, Dice.Two, Dice.Two
    let category = Category.Twos
    score throw category
    |> should equal 10

[<Fact>]
let ``Five dices with two placed in threes category`` ()= 
    let throw = Dice.Two, Dice.Two, Dice.Two, Dice.Two, Dice.Two
    let category = Category.Threes
    score throw category
    |> should equal 0

[<Fact>]
let ``Five dices with three placed in threes category`` ()= 
    let throw = Dice.Three, Dice.Three, Dice.Three, Dice.Three, Dice.Three
    let category = Category.Threes
    score throw category
    |> should equal 15

[<Fact>]
let ``Five dices with pair of twos and rest ones placed in pair category`` ()= 
    let throw = Dice.One, Dice.One, Dice.One, Dice.Two, Dice.Two
    let category = Category.Pair
    score throw category
    |> should equal 4

[<Fact>]
let ``Five dices with one placed in pair category`` ()= 
    let throw = Dice.One, Dice.One, Dice.One, Dice.One, Dice.One
    let category = Category.Pair
    score throw category
    |> should equal 2

