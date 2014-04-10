module FsYahtzee

open Xunit
open FsUnit.Xunit

type Dice = One | Two 

type Category = Ones

let score (dOne, dTwo, dThree, dFour, dFive) category =
    [dOne; dTwo; dThree; dFour; dFive] |> List.filter (fun x -> x = Dice.One) |> List.length

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
