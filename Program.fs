// Learn more about F# at http://fsharp.org
namespace FSharpCode

#nowarn "40" // Fib warning

open System
open System.IO
open System.Collections.Generic
open System.Net
open RabbitMQ.Client
open BasicFunctions
open NUnit.Framework
open MathNet.Numerics
open System.Text.RegularExpressions
open System.Net.Http
open System.Threading;
open System.Threading.Tasks;
open System.Numerics

[<Measure>] type ft
[<Measure>] type sqft = ft ^ 2

[<Measure>] type dpi

module QuerySource =
    type Film = 
        { Id : int
          Name : string
          ReleaseYear : int
          Gross : Nullable<float> }
        override x.ToString() = 
            sprintf "%s (%i)" x.Name x.ReleaseYear
    
    type Actor = 
        { Id : int
          FirstName : string
          LastName : string }
        override x.ToString() = 
            sprintf "%s, %s" x.LastName x.FirstName
    
    type FilmActor = 
        { FilmId : int
          ActorId : int }

    /// Given a radius, calculate the diameter, area, and circumference
    /// of a circle
    let measureCircle radius =
        let diameter = radius * 2.0
        let area = Math.PI * (radius ** 2.0)
        let circumference = 2.0 * Math.PI * radius
        (diameter, area, circumference)
    
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
    type Msg = 
        | Incr of int
        | Fetch of AsyncReplyChannel<int>
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

    type Guy(id: Guid, name: string, age: int) =
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
    
    type IWeapon =
        abstract Description: string with get
        abstract Power: int with get
    
    type Character(name: string, maxHP: int) =
        member x.Name = name
        member val HP = maxHP with get, set
        member val Weapon: IWeapon option = None with get, set
        member x.Attack(o: Character) =
            let power = match x.Weapon with
                        | Some(w) -> w.Power
                        | None -> 1
            o.HP <- Math.Max(0, o.HP - power)
        static member SayWeapon() =
            printfn "%A" Character.Tiet
        static member Tiet = "Land Wears"
        override x.ToString() =
            sprintf "%s: %i/%i" name x.HP maxHP

    type Person(name : string) =
        let mutable _name = name;
        let nameChanged = new Event<unit>() (* creates event *)
        
        member this.NameChanged = nameChanged.Publish (* exposed event handler *)
        
        member this.Name
            with get() = _name
            and set(value) =
                _name <- value
                nameChanged.Trigger() (* invokes event handler *)

    
    type MyException(message, category) =
        inherit exn(message)
        member x.Category = category
        override x.ToString() = sprintf "[%s] %s" category message

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
        let negate x = -x
        List.map    (fun i -> i+1) [0;1;2;3] |> printfn "%A"
        List.filter (fun i -> i>1) [0;1;2;3] |> printfn "%A"
        List.sortBy (fun i -> (negate i)) [0;1;2;3] |> printfn "%A"

        let myMat = LinearAlgebra.Matrix<double>.Build.Random(10,10)
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

        let me = Guy(Guid.NewGuid(), "Dave", 23)
        me |> printfn "%A"

        let yellow = RgbColor(255, 255, 0)
        -yellow |> printfn "%A" // Blue color

        let middleName: string option = None
        middleName |> printfn "%A"
        let middleName = Some("William")
        middleName |> printfn "%A"

        let witchKing = Character("Witch-king", 100)
        let frodo = Character("Frodo", 50)

        Character.SayWeapon |> ignore
        Character.Tiet |> printfn "%A"

        let p = new Person("Bob")
        p.NameChanged.Add(fun () -> printfn "-- Name changed! New name: %s" p.Name)

        printfn "Event handling is easy"
        p.Name <- "Joe"

        printfn "It handily decouples objects from one another"
        p.Name <- "Moe"

        p.NameChanged.Add(fun () -> printfn "-- Another handler attached to NameChanged!")

        printfn "It's also causes programs behave non-deterministically."
        p.Name <- "Bo"

        printfn "The function NameChanged is invoked effortlessly."

        // Regular Expressions
        let (=~) input pattern =
            Regex.IsMatch(input, pattern)
        printfn "cat =~ dog: %b" ("cat" =~ "dog")
        printfn "cat =~ cat|dog: %b" ("cat" =~ "cat|dog")
        printfn "monkey =~ monk*: %b" ("monkey" =~ "monk*")

        // let mutable guess = String.Empty
        // guess <- Console.ReadLine()
        // Console.WriteLine(String.Format("You didn't guess the password {0}", guess))
        
        // let uri = "http://google.com"
        // let c = new HttpClient()
        // let workThenWait() =
        //     Thread.Sleep(1000) |> ignore
        //     printfn "work done"
        //     async { do c.GetStringAsync(uri).Result |> printfn "%A" }
        //     // async { do c.GetAsync(uri).Result |> printfn "%A" }
        // let work = workThenWait() |> Async.StartAsTask
        // printfn "started"
        // work.Wait()
        // printfn "completed"

        try
            raise <| MyException("blah", "debug")
        with
            | :? MyException as ex -> printfn "My Exception: %s" <| ex.ToString() | ex -> printfn "General Exception: %s" <| ex.ToString()
        
        QuerySource.measureCircle 20.0 |> printfn "%A"

        let poem = "The lesser world was daubed\n\
            By a colorist of modest skill\n\
            A master limned you in the finest inks\n\
            And with a fresh-cut quill.\n"
        printfn "%A" poem

        let multiLineStr = "\
Good things
And then"
        printfn "%A" multiLineStr

        let cplx = new Complex(1.0,2.0)
        printfn "%A" cplx

        // Basic Function Composition
        let square x = x * x
        let toString (x : int) = x.ToString()
        let strLen (x : string) = x.Length
        let lenOfSquare = square >> toString >> strLen

        square 128 |> printfn "%A"
        lenOfSquare 128 |> printfn "%A"

        printfn "MailboxProcessor example"
        let counter = 
            MailboxProcessor.Start(fun inbox ->
                let rec loop n =
                    async {
                        let! msg = inbox.Receive()
                        match msg with
                        | Incr(x) -> return! loop(n + x)
                        | Fetch(replyChannel) ->
                            replyChannel.Reply(n)
                            return! loop(n)
                    }
                loop 0
            )
        counter.Post(Incr 7)
        counter.Post(Incr 50)
        counter.PostAndReply(Fetch) |> printfn "%A"
        0 // return an integer exit code