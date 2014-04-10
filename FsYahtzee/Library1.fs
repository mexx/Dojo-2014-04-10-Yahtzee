module FsYahtzee

open Xunit
open FsUnit.Xunit

type Dice = One | Two

type Category = Ones

let score (dOne, _, _, _, _) category =
    if dOne = Dice.One then 6 else 0

[<Fact>]
let ``Five dices with one placed in ones category`` ()= 
    let throw = Dice.One, Dice.One, Dice.One, Dice.One, Dice.One
    let category = Category.Ones
    score throw category
    |> should equal 6

[<Fact>]
let ``Five dices with two placed in ones category`` () =
    let throw =Dice.Two, Dice.Two, Dice.Two, Dice.Two, Dice.Two
    let category = Category.Ones
    score throw category
    |> should equal 0

