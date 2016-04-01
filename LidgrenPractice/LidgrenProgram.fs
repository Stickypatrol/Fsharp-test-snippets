open System
open System.Net
open Lidgren.Network
open CoroutineMonad
open AuxiliaryTypes
(*
type Mode =
  | Create = 0
  | Join   = 1

type MessageType =
  | PeerInformation = 0
  | Data = 1

type Incoming = NetIncomingMessageType

[<EntryPoint>]
let main args =
  let boot() =
    let rec getMode () =
        printfn "0 - Create, 1 - Join"
        match Console.ReadLine () with
        | "0" ->
            printfn "Creating network"
            Mode.Create
        | "1" ->
            printfn  "Joining network"
            Mode.Join
        | _   ->
            printfn "Invalid input!"
            getMode ()
    let mode = getMode ()
    let localPort =
        printfn "Type a local port number to use"
        Int32.Parse(Console.ReadLine ())

    printfn "Using local port: %A" localPort
    
    let config = new NetPeerConfiguration "Networking Example"
    do config.Port                      <- localPort
    do config.MaximumConnections        <- 128
    do config.LocalAddress              <- new IPAddress ((int64)0x0100007f) //The hex value for 127.0.0.1(localhost) //NetUtility.Resolve("localhost")
    do config.AcceptIncomingConnections <- true
    let messageTypes =
        [ Incoming.DiscoveryRequest
          Incoming.DiscoveryResponse
          Incoming.ConnectionApproval
          Incoming.StatusChanged
          Incoming.UnconnectedData
          Incoming.Data
          Incoming.VerboseDebugMessage
          Incoming.DebugMessage
          Incoming.WarningMessage
          Incoming.ErrorMessage
          ]
    let rec enableMessageTypes list =
        match list with
        | x :: xs ->
            do config.EnableMessageType x
            enableMessageTypes xs
        | []      -> ()
    enableMessageTypes messageTypes
    printfn "Starting peer"
    let peer = new NetPeer (config)
    peer.Start ()
    
    let remotePort =  if mode = Mode.Join then
                        printfn "Type a remote port number to use"
                        Int32.Parse (Console.ReadLine())
                      else
                        0
    printfn "Using remote port: %A" remotePort
    let handleDebugMessage (message : NetIncomingMessage) =
        printfn "Debug: %s" (message.ReadString ())
      

  let rec mainLoop() =
    ()
  boot()
  mainLoop()
  0*)