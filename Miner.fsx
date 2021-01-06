// Learn more about F# at http://fsharp.org
#if INTERACTIVE
#time "on"
#r "nuget: Akka.Fsharp"
#endif

open Akka.Actor
open Akka.FSharp
open System
open System.Security.Cryptography
open System.Text

type BossMessage =
    | Mine of int * int * string
    | Done of string * string

type MinerMessage =
    | GenerateCoin of int * int * string

let system: Akka.Actor.ActorSystem =
    create "miner" <| Configuration.defaultConfig ()

let hash (coin: string): string =
    let sha256 = SHA256Managed.Create ()
    coin
    |> Encoding.ASCII.GetBytes
    |> sha256.ComputeHash
    |> Array.map (sprintf "%02x")
    |> String.concat ""

let isBitcoin (coin: string) (k: int): bool =
    let zeros = String.replicate k "0"
    let prefix = coin.[0 .. k-1]
    zeros = prefix

let generateCoinSuffix (n: int): string =
    let chars = "abcdefghijklmnopqrstuvwxyz0123456789"
    let random = Random ()
    [| for _ in 1..n -> chars.[ random.Next chars.Length ] |]
    |> String

let generateBitcoin (k: int) (n: int) (id: string): string * string =
    let rec generateCoin (): string * string =
        let coin = id + generateCoinSuffix n
        let hash = hash coin
        match isBitcoin hash k with
        | true ->
            coin, hash
        | false ->
            generateCoin ()
    generateCoin ()

let miner (i: int): IActorRef =
    spawn system
    <| sprintf "miner-%d" i
    <| fun (mailbox: Actor<MinerMessage>) ->
        let rec loop () =
            actor {
                match! mailbox.Receive () with
                | GenerateCoin (k, n, id) ->
                    mailbox.Sender () <! Done (generateBitcoin k n id)
                return! loop ()
            }
        loop ()

let minerBoss (miners: int): IActorRef =
    spawn system "boss"
    <| fun (mailbox: Actor<BossMessage>) ->
        let rec loop (index: int) (main: IActorRef) =
            actor {
                match! mailbox.Receive () with
                | Mine (k, n, id) ->
                    [ 1 .. miners ]
                    |> List.map miner
                    |> List.iter (fun miner -> miner <! GenerateCoin (k, n, id))
                    return! loop index <| mailbox.Sender ()
                | Done (coin, hash) ->
                    mailbox.Sender () <! PoisonPill.Instance
                    printfn "%s  %s" coin hash
                    match index + 1 with
                    | completed when completed = miners ->
                        main <! 0
                    | _ -> return! loop <| index + 1 <| main
            }
        loop 0 null

// Number of zeros
let k = fsi.CommandLineArgs |> Array.tail |> Array.head |> int
let boss = minerBoss 18

printf "\n"
Async.RunSynchronously (boss <? Mine (k, 16, "tbelani;"), -1)
printf "\n"

boss <! PoisonPill.Instance
