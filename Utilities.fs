namespace FSharpCode

open System

module BasicFunctions =

    // Use 'let' to define a function that accepts an integer argument and returns an integer.
    let func1 x = x*x + 3

    // Parenthesis are optional for function arguments
    let func1a (x) = x*x + 3

    /// Apply the function, naming the function return result using 'let'.
    /// The variable type is inferred from the function return type.
    let result1 = func1 4573
    printfn "The result of squaring the integer 4573 and adding 3 is %d" result1

    // When needed, annotate the type of a parameter name using '(argument:type)'
    let func2 (x:int) = 2*x*x - x/5 + 3

    let result2 = func2 (7 + 4)
    printfn "The result of applying the 1st sample function to (7 + 4) is %d" result2

    let func3 x =
        if x < 100.0 then
            2.0*x*x - x/5.0 + 3.0
        else
            2.0*x*x + x/5.0 - 37.0

    let result3 = func3 (6.5 + 4.5)
    printfn "The result of applying the 2nd sample function to (6.5 + 4.5) is %f" result3

    let name = "Piyush"
    name.CompareTo "Chauhan" |> printfn "%d"
    let revString = name.ToCharArray() |> Array.rev |> String
    let nRev = 
        name
        |> Seq.toArray
        |> Array.rev
        |> String
    
    printfn "%s" revString.[1..3]
    printfn "%s" nRev.[1..3]

module Utilities = 
    let func1 x = x*x + 3