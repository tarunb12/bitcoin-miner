// Learn more about F# at http://fsharp.org
#time "on"
#r "nuget: Akka.Fsharp"
#r "nuget: Akka.Remote"

open Akka.FSharp
open System
open System.Security.Cryptography
open System.Text

type BossMessage =
    | Work of int * int * string * int
    | Done of string * string

type WorkerMessage =
    | GenerateCoin of int * int * string

let system: Akka.Actor.ActorSystem =
    create "miner"
    <| Configuration.defaultConfig ()

let hash (coin: string): string =
    let sha256 = new SHA256Managed ()
    coin
    |> Encoding.ASCII.GetBytes
    |> sha256.ComputeHash
    |> Array.map (sprintf "%02x")
    |> String.concat ""

let isBitcoin (coin: string) (k: int): bool =
    let zeros = String.replicate k "0"
    let prefix = coin.[0 .. k-1]
    zeros = prefix

let generateCoinSuffix (n: int) (id: string): string =
    let chars = "abcdefghijklmnopqrstuvwxyz0123456789"
    let random = Random ()
    let length = n - id.Length
    [| for _ in 1..length -> chars.[ random.Next chars.Length ] |]
    |> string

let worker (i: int): Akka.Actor.IActorRef =
    spawn system
    <| sprintf "worker%d" i
    <| fun (mailbox: Actor<WorkerMessage>) ->
        let rec loop () =
            actor {
                let! message = mailbox.Receive ()
                match message with
                | _ -> ignore () // mailbox.Sender () <! ...
                return! loop ()
            }
        loop ()

let boss: Akka.Actor.IActorRef =
    spawn system "boss"
    <| fun (mailbox: Actor<BossMessage>) ->
        let rec loop () =
            actor {
                let! message = mailbox.Receive ()
                match message with
                | Work (k, n, id, cores) ->
                    List.map worker [ 1 .. cores ]
                    |> List.iter (fun worker -> worker <! GenerateCoin (k, n, id))
                | Done (coin, hash) ->
                    printfn "%s  %s" coin hash
                return! loop ()
            }
        loop ()

// Number of zeros
let k = fsi.CommandLineArgs |> Array.tail |> Array.head |> int

// printfn "%s" <| hash "adobra;kjsdfk11"
printf "\n"
// boss <! Work (k, 16, "tbelani;", Environment.ProcessorCount)
printf "\n"
// ActorRef ! Broadcast(PoisonPill)
