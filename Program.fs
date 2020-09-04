// Learn more about F# at http://fsharp.org

[<EntryPoint>]
let main (argv: string []): int =
    let k = int argv.[0]
    printfn "Hello World from F#! %d" k
    0 // return an integer exit code
