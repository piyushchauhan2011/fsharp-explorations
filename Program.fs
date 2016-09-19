// Learn more about F# at http://fsharp.org
namespace FSharpCode

open System
open RabbitMQ.Client
open BasicFunctions

module Program =
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
        0 // return an integer exit code
        