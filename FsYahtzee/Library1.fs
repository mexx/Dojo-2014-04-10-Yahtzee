module FsYahtzee

open Xunit
open FsUnit.Xunit

type Dice = One | Two 

type Category = Ones | Twos

let score (dOne, dTwo, dThree, dFour, dFive) category =
    let diceToCount, diceScore =
        match category with
        | Ones -> One, 1
        | Twos -> Two, 2

    [dOne; dTwo; dThree; dFour; dFive]
    |> List.filter ((=) diceToCount)
    |> List.length
    |> (*) diceScore

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
