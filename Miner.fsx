// Learn more about F# at http://fsharp.org
#time "on"
#r "nuget: Akka.Fsharp"
#r "nuget: Akka.Remote"

open Akka.FSharp
open System
open System.Security.Cryptography
open System.Text

type BossMessage =
    | Zeros of int
    | Done of string * string

type WorkerMessage =
    | Work

// Prefix ID
let id = "tbelani;"
// Number of zeros
let k = fsi.CommandLineArgs |> Array.tail |> Array.head |> int
// Zeros coin prefix
let zeros = String.replicate k "0"
// Number of cores
let cores = Environment.ProcessorCount
// Coin length
let n = 16

let system: Akka.Actor.ActorSystem =
    System.create "miner" <| Configuration.defaultConfig ()

let hash (coin: string): string =
    coin
    |> Encoding.ASCII.GetBytes
    |> (new SHA256Managed ()).ComputeHash
    |> Array.map (sprintf "%02x")
    |> String.concat ""

let isBitcoin (coin : string) : bool =
    let prefix = coin.[0 .. k-1]
    String.Equals (prefix, zeros)

let generateCoinSuffix () =
    let chars = "abcdefghijklmnopqrstuvwxyz0123456789"
    let random = Random ()
    let length = n - id.Length
    [| for _ in 1..length -> chars.[ random.Next chars.Length ] |]
    |> string

let worker (i : int) : Akka.Actor.IActorRef =
    spawn system (sprintf "worker%d" i) <| fun (mailbox : Actor<WorkerMessage>) ->
        let rec loop () =
            actor {
                let! message = mailbox.Receive ()
                match message with
                | _-> ignore ()
                return! loop ()
            }
        loop ()

let boss : Akka.Actor.IActorRef =
    spawn system "boss" <| fun (mailbox : Actor<BossMessage>) ->
        let rec loop () =
            actor {
                let! message = mailbox.Receive ()
                match message with
                | Zeros k ->
                    List.map worker [ 1 .. cores ]
                    |> List.iter (fun worker -> worker <! Work)
                | Done (coin, hash) ->
                    printfn "%s %s" coin hash
                return! loop ()
            }
        loop ()

printfn "# of cores: %d" cores
printfn "%s" (hash "adobra;kjsdfk11")
// boss <! k
//     ActorRef ! Broadcast(PoisonPill)
