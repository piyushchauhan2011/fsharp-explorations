// Learn more about F# at http://fsharp.org
namespace FSharpCode

open System
open System.IO
open System.Net
open RabbitMQ.Client
open BasicFunctions
open NUnit.Framework

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

        let data = seq {
            use fs = new FileStream(@"ProgrammingLanguages.csv", FileMode.Open)
            use sr = new StreamReader(fs)
            while not sr.EndOfStream do
                yield sr.ReadLine()
        }
        data |> printfn "%A"
        
        0 // return an integer exit code