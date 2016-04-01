module AuxiliaryTypes


open CoroutineMonad
//typical message for network/local communication between messages
type Message = string*int
(*  | Local of Contents
  | Remote of Contents
  with
  static member AddToOutList outgoing id (msg:Message) =
    match Map.tryFind id outgoing with
    | Some(curList) -> Map.add id (msg::curList) outgoing
    | None -> Map.add id [msg] outgoing
  
  static member AddToInList incoming id (msg:Message) =
    match Map.tryFind id incoming with
    | Some(curList) -> Map.add id (msg::curList) incoming
    | None -> Map.add id [msg] incoming
and Contents =
  | MUpdate of int*NetBody                         //0
  | MCollide of int*int                            //1
  | MCreate of EntityType * int * string * NetBody //2
  | MDestroy of int                                //3
and NetBody = float*float*float*float*float
and EntityType =
  | TPlayer
  | TAsteroid
  | TProjectile
*)
and MessageType =
    | GameData  = 1
    | Text      = 2

and MessageMap = Map<int, List<Message>>
//                   ID , List<actualmessage>

and MessageCollection =
  {
    Incoming : MessageMap
    Outgoing : MessageMap
  } with
  static member LocalMessage id msg =
    fun w so si ->
      Done({Incoming = so.Incoming; Outgoing = Map.add id [msg] so.Outgoing}, si, ())
  
  static member RemoteMessage id msg =
    fun w so si ->
      Done({Incoming = so.Incoming; Outgoing = Map.add id [msg] so.Outgoing}, si, ())
  static member ListtoMap (msgMap:MessageMap) (msgList:(int*Message) list) =
    List.fold (fun (msgmap:MessageMap) (id:int, msg:Message) -> match Map.tryFind id msgmap with
                                                                | Some(msgList) -> Map.add id (msg::msgList) msgmap
                                                                | None -> Map.add id [msg] msgmap) msgMap msgList

type Mode =
  | Create = 0
  | Join = 1