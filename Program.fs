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

[<Measure>] type ft
[<Measure>] type sqft = ft ^ 2

[<Measure>] type dpi

module QuerySource =
    type Film = { Id : int; Name : string; ReleaseYear : int; Gross : Nullable<float> }
                override x.ToString() = sprintf "%s (%i)" x.Name x.ReleaseYear
    type Actor = { Id : int; FirstName : string; LastName : string }
                 override x.ToString() = sprintf "%s, %s" x.LastName x.FirstName
    type FilmActor = { FilmId : int; ActorId : int }
    
    let films =
        [
        { Id = 1; Name = "The Terminator"; ReleaseYear = 1984; Gross = Nullable 38400000.0 }
        { Id = 2; Name = "Predator"; ReleaseYear = 1987; Gross = Nullable 59735548.0 }
        { Id = 3; Name = "Commando"; ReleaseYear = 1985; Gross = Nullable<float>() }
        { Id = 4; Name = "The Running Man"; ReleaseYear = 1987; Gross = Nullable 38122105.0 }
        { Id = 5; Name = "Conan the Destroyer"; ReleaseYear = 1984; Gross = Nullable<float>() } ]
    
    let actors =
        [
        { Id = 1; FirstName = "Arnold"; LastName = "Schwarzenegger" }
        { Id = 2; FirstName = "Linda"; LastName = "Hamilton" }
        { Id = 3; FirstName = "Carl"; LastName = "Weathers" }
        { Id = 4; FirstName = "Jesse"; LastName = "Ventura" }
        { Id = 5; FirstName = "Vernon"; LastName = "Wells" } ]

    let filmActors =
        [ 
        { FilmId = 1; ActorId = 1 }
        { FilmId = 1; ActorId = 2 }
        { FilmId = 2; ActorId = 1 }
        { FilmId = 2; ActorId = 3 }
        { FilmId = 2; ActorId = 4 }
        { FilmId = 3; ActorId = 1 }
        { FilmId = 3; ActorId = 5 }
        { FilmId = 4; ActorId = 1 }
        { FilmId = 4; ActorId = 4 } ]

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

    type Person (id: Guid, name: string, age: int) =
        member x.Id = id
        member x.Name = name
        member x.Age = age
        override x.ToString() = sprintf "%A %s %d" x.Id x.Name x.Age

    type RgbColor(r: int, g: int, b: int) =
        member x.Red = r
        member x.Green = g
        member x.Blue = b
        override x.ToString() = sprintf "%i, %i, %i" r g b

        static member (~-) (r: RgbColor) =
            RgbColor(
                r.Red ^^^ 0xFF,
                r.Green ^^^ 0xFF,
                r.Blue ^^^ 0xFF
            )

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
        let infSeq n = seq { 1I..n }
        infSeq 1000000000000I |> printfn "%A"
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
        Parallel.For(0, 100, printf "%i ") |> printfn "%A"

        let length = 10.0<ft>
        let area = 10.0<sqft>
        printfn "Area: %A" area

        let resolution = 300.0 * 1.0<dpi>
        printfn "Resolution: %A" resolution
        
        query { for f in QuerySource.films do select f } |> printfn "%A"

        Parallel.Invoke(
            (fun () -> printfn "Task 1"),
            (fun () -> Task.Delay(500).Wait()
                       printfn "Task 2"),
            (fun () -> printfn "Task 3")
        )
        printfn "Done"

        let me = Person(Guid.NewGuid(), "Dave", 23)
        me |> printfn "%A"

        let yellow = RgbColor(255, 255, 0)
        -yellow |> printfn "%A" // Blue color

        let middleName: string option = None
        middleName |> printfn "%A"
        let middleName = Some("William")
        middleName |> printfn "%A"
        0 // return an integer exit code