// Learn more about F# at http://fsharp.org
namespace FSharpCode

#nowarn "40"

open System
open System.IO
open System.Collections.Generic
open System.Net
open System.Threading.Tasks
open RabbitMQ.Client
open BasicFunctions
open NUnit.Framework
open MathNet.Numerics.LinearAlgebra

module Program =
    // interface
    type IEnumerator<'a> =
        abstract member Current: 'a
        abstract MoveNext: unit -> bool
    
    // abstract base class with virtual methods
    [<AbstractClass>]
    type Shape() =
        //readonly properties
        abstract member Width: int with get
        abstract member Height: int with get
        //non-virtual method
        member this.BoundingArea = this.Height * this.Width
        //virtual method with base implementation
        abstract member Print: unit -> unit
        default this.Print() = printfn "I'm a shape"
    
    type Rectangle(x:int, y:int) =
        inherit Shape()
        override this.Width = x
        override this.Height = y
        override this.Print() = printfn "I'm a Rectangle"

    [<Test>]
    let ``When 2 is added to 2 expect 4``() = 
        Assert.AreEqual(4, 2+2)

    [<Test>]
    let ``When List is added to list expect length to increment by 1``() = 
        Assert.AreEqual(11, 11 :: [1..10] |> List.length)
    
    [<EntryPoint>]
    let main argv = 
        printfn "Hello World!"
        printfn "%A" argv
        
        let sb = 23y
        let by = 25uy
        printfn "%i %i" sb by

        let arbrn = 987238947982340982309480928340982309480923840982340982309480923840923840983209489089032840982340978923749876387947598437598739804709837589037459083740598734098570394875908347509834759083475980347095873490857304985I
        printfn "%A" (arbrn ** 2)
        
        printfn "%d" (func1 4573)

        let r = Rectangle(2, 3)
        printfn "The width is %i" r.Width
        printfn "The area is %i" r.BoundingArea
        r.Print()

        let filename = @"ProgrammingLanguages.csv"
        let xsvEnumerator fileName = 
            seq {
                use fs = new FileStream(fileName, FileMode.Open)
                use s = new StreamReader(fs)
                while not s.EndOfStream do
                    let line = s.ReadLine()
                    let tokens = line.Split [|'\t'|]
                    yield tokens
            }
        let xsv = xsvEnumerator(filename)
        // xsv |> Seq.iter (string >> printfn "line %s")
        xsv |> Seq.iter (Array.length >> printfn "line has %d entries")
        xsv |> Seq.iter (Array.map (fun s -> s.Length) >> printfn "lengths of entries: %A")
        xsv |> Seq.iter (Array.map (fun s -> s.ToString()) >> printfn "Entries: %A")
        xsv |> printfn "%A"
        
        let printTwoParameters x y = 
            printfn "x=%i y=%i" x y
        
        // step by step version
        let x = 6
        let y = 99
        let intermediateFn = printTwoParameters x  // return fn with // x "baked in"
        let result  = intermediateFn y
        let infSeq n = seq { 1..n }
        infSeq 10 |> printfn "%A"
        // Partial Application
        List.map    (fun i -> i+1) [0;1;2;3] |> printfn "%A"
        List.filter (fun i -> i>1) [0;1;2;3] |> printfn "%A"
        List.sortBy (fun i -> -i) [0;1;2;3] |> printfn "%A"

        let myMat = Matrix<double>.Build.Random(10,10)
        let myMat' = myMat.Inverse()
        printfn "%A" myMat'
        
        let rec fib = 
            let dict = new Dictionary<_,_>()
            fun n ->
                match dict.TryGetValue(n) with
                | true, v -> v
                | false, _ -> 
                    let temp =
                        if n = 0I then 0I
                        elif n = 1I then 1I
                        else fib (n - 1I) + fib(n - 2I)
                    dict.Add(n, temp)
                    temp
        fib(100I) |> printfn "%A"

        let seqInfinite = Seq.initInfinite (fun index ->
            let n = float( index + 1 )
            1.0 / (n * n * (if ((index + 1) % 2 = 0) then 1.0 else -1.0)))
        printfn "%A" seqInfinite
        seqInfinite |> Seq.take 2 |> printfn "%A"

        let movies = array2D [| [|"The Terminator"; "1984"|]
                                [|"Predator"; "1987"|]
                                [| "Commando"; "1985" |]
                                [| "The Running Man"; "1987" |]
                                [| "True Lies"; "1994" |]
                                [| "Last Action Hero"; "1993" |]
                                [| "Total Recall"; "1990" |]
                                [| "Conan the Barbarian"; "1982" |]
                                [| "Conan the Destroyer"; "1984" |]
                                [| "Hercules in New York"; "1969" |]
                             |]
        movies |> printfn "%A"

        query { for n in [1..100] do 
                where (n%2 = 0)
                sortByDescending n } |> printfn "%A"
        Parallel.For(0, 100, printf "%i ") |> ignore
        0 // return an integer exit code