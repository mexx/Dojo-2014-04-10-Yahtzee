module FsYahtzee

open Xunit
open FsUnit.Xunit

type Dice = One

type Category = Ones

let score throw category =
    6

[<Fact>]
let ``Six dices with one placed in ones category`` ()= 
    let throw = [Dice.One; Dice.One; Dice.One; Dice.One; Dice.One; Dice.One]
    let category = Category.Ones
    score throw category
    |> should equal 6