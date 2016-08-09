// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
open System
open System.IO
open System.Net
open System.Net.Sockets
open System.Text
open System.Threading
open System.Collections.Generic
open System.Runtime.Serialization

open FSharp.Data

// INFO: "Fleece.FSharpData" install in NuGet


/// Object to Json 
let internal json<'t> (myObj:'t) =   
    use ms = new MemoryStream() 
    (new DataContractJsonSerializer(typeof<'t>)).WriteObject(ms, myObj) 
    Encoding.Default.GetString(ms.ToArray()) 

let int2String (x: int) = string x

type KeyObject(number : int, px : float, py: float, color: string) = class
    let mutable PosX: float = px
    let mutable PosY: float = py

    member x.Id = int
    //member x.PosX = px
    //member x.PosY = py
    member x.Color = color

    member x.SetPosition(xx: float, yy: float) =
        PosX = xx 
        PosY = yy
        x
end

[<AbstractClass; Sealed>]
type StaticRealObjects private () =
    static member objects : Map<int, KeyObject> = Map.empty
    static member GetObject(idx: int) =
        StaticRealObjects.objects.[idx]
    static member UpdatePosition(idx: int, px: float, py: float) =
        StaticRealObjects.objects.[idx].SetPosition(px, py)
    static member AddObject(o: KeyObject) =
        let id = o.Id 0
        StaticRealObjects.objects.Add(id , o)

type ObjectUser(id : int, posX : float, posY: float) = class
    //let mutable amount = 0m
 
    member x.Id = id
    member x.PosX = posX
    member x.PosY = posY
    member x.Map = 0
    member x.RollDie = 0
    member x.Objects : Set<int> = Set.empty
 
    member x.AddObject(id_obj: int)= 
        x.Objects.Add(id_obj)
    member x.RemoveObject(id_obj: int) =
        x.Objects.Remove(id_obj)
end

[<AbstractClass; Sealed>]
type StaticPositions private () =
    static member positions : Map<string, ObjectUser> = Map.empty

    static member AddUser(ou: ObjectUser) =
        StaticPositions.positions.Add(int2String ou.Id, ou)
        ""

    static member GetUser(id: int) =
        StaticPositions.positions.[int2String id]


type Map(id: int, fields: string, width: int, height: int) = class // Falta objects )
    member x.Id = id
    member x.MapFields = fields
    member x.Width = width
    member x.Height = height
    member x.KeyObjects : Map<string,KeyObject> = Map.empty 

    member x.AddKey(idx: int, px: float, py: float) = 
        StaticRealObjects.UpdatePosition(idx, px, py)
        let obj = StaticRealObjects.GetObject(idx)
        x.KeyObjects.Add(idx.ToString(), obj)
        obj
    member x.RemoveKey(idx: int) =
        x.KeyObjects.Remove(idx.ToString())
        StaticRealObjects.GetObject(idx)
end

type Counter() = class 
    member x.counter: int = 0
    member x.Add =
        x.counter = x.counter + 1
        x.counter
end

let counter = new Counter()

// Enhance the TcpListener class so it can handle async connections
type System.Net.Sockets.TcpListener with
    member x.AsyncAcceptTcpClient() =
        Async.FromBeginEnd(x.BeginAcceptTcpClient, x.EndAcceptTcpClient)

// Type that defines protocol for interacting with the ClientTable
type ClientTableCommands = 
    | Add of (string * StreamWriter)
    | Remove of string
    | SendMessage of (string * string)

let GetAction(j_msg: JsonValue) =
    let v = j_msg.TryGetProperty("Action")
    if v.IsNone then
        ""
    else
        v.Value.AsString(Globalization.CultureInfo.CurrentCulture)

// A class that will store a list of names of connected clients along with 
// streams that allow the client to be written too
type ClientsTable() =
    // create the mail box 
    let mailbox = MailboxProcessor.Start(fun inbox ->
        // main loop that will read messages and update the
        // client name/stream writer map
        let rec loop (clients: Map<string, StreamWriter>) =
            async { let! msg = inbox.Receive()
                    match msg with
                    | Add (name, sw) ->
                        return! loop (Map.add name sw clients)
                    | Remove name -> 
                        return! loop (Map.remove name clients)
                    | SendMessage (msg, name) -> 
                        try
                            let j_msg = JsonValue.Parse(msg)
                            let action = GetAction j_msg
                            match action with
                            | "initWName" ->
                                let ou = new ObjectUser(1, 5.0*64.0, 5.0*64.0)
                                StaticPositions.AddUser(ou)
                                JsonValue.Load(ou)
                                printf("initWName")
                            | "move" -> 
                                printf("move")
                            | "position" -> 
                                printf("position")
                            | "fight" -> 
                                printf("fight")
                            | "finishBattle" -> 
                                printf("finishBattle")
                            | "getObj" -> 
                                printf("getObj")
                            | "freeObj" -> 
                                printf("freeObj")
                            | "exit" -> 
                                printf("exit")
                            | _ ->  printf("LOL")
                            for (n, sw) in Map.toSeq clients do
                                try
                                    if (n <> name) then
                                        sw.WriteLine msg
                                        sw.Flush()
                                with _ -> ()
                        with
                            | :? System.Exception-> printf("Imrpime")
                        //try
                        //    let j_msg = JsonValue.Parse(msg)
                            //let action = GetAction j_msg
                            //match action with
                            //| "move" -> printf("move")
                            //| "position" ->  printf("position")
                            //| "fight" ->  printf("fight")
                            //| "finishBattle" ->  printf("finishBattle")
                            //| "getObj" ->  printf("getObj")
                            //| "freeObj" ->  printf("freeObj")
                            //| "exit" ->  printf("exit")
                            //| _ ->  printf("LOL")
                        //with _ -> printf("ERROR")
                        return! loop clients }
        // start the main loop with an empty map
        loop Map.empty)
    /// add a new client
    member x.Add(name, sw) = mailbox.Post(Add(name, sw))
    /// remove an existing connection
    member x.Remove(name) = mailbox.Post(Remove name)
    /// handles the process of sending a message to all clients
    member x.SendMessage(msg, name) = mailbox.Post(SendMessage(msg, name))

/// perform async read on a network stream passing a continuation 
/// function to handle the result 
let rec asyncReadTextAndCont (stream: NetworkStream) cont  =
    // unfortunatly we need to specific a number of bytes to read
    // this leads to any messages longer than 512 being broken into
    // different messages
    async { let buffer = Array.create 512 0uy
            let! read = stream.AsyncRead(buffer, 0, 512) 
            let allText = Encoding.UTF8.GetString(buffer, 0, read)
            return cont stream allText  }

// class that will handle client connections
type Server() =

    // client table to hold all incoming client details
    let clients = new ClientsTable()
    
    // handles each client
    let handleClient (connection: TcpClient) =
        // get the stream used to read and write from the client
        let stream = connection.GetStream()
        // create a stream write to more easily write to the client
        let sw = new StreamWriter(stream)

        let rec requestAndReadName (stream: NetworkStream) (name: string) =
            // read the name
            let name = name.Replace(Environment.NewLine, "")
            // main loop that handles conversations
            let rec mainLoop (stream: NetworkStream) (msg: string) = 
                try
                    // send received message to all clients
                    let msg = Printf.sprintf "%s" msg
                    clients.SendMessage(msg, name)
                with _ ->
                    // any error reading a message causes client to disconnect
                    clients.Remove name
                    sw.Close()
                Async.Start (asyncReadTextAndCont stream mainLoop)
            clients.Add(name, sw)
            Async.Start (asyncReadTextAndCont stream mainLoop)
        // welcome the new client by printing "What is you name?"
        sw.WriteLine("What is your name? "); 
        sw.Flush()
        // start the main loop that handles reading from the client
        Async.Start (asyncReadTextAndCont stream requestAndReadName)
        
        // handles reading the name then starts the main loop that handles
        // conversations
        (*let rec requestAndReadName (stream: NetworkStream) (name: string) =
            // read the name
            let name = name.Replace(Environment.NewLine, "")
            // main loop that handles conversations
            let rec mainLoop (stream: NetworkStream) (msg: string) = 
                try
                    // send received message to all clients
                    let msg = Printf.sprintf "%s: %s" name msg
                    clients.SendMessage msg
                with _ ->
                    // any error reading a message causes client to disconnect
                    clients.Remove name
                    sw.Close()
                Async.Start (asyncReadTextAndCont stream mainLoop)
            clients.Add(name, sw)
            Async.Start (asyncReadTextAndCont stream mainLoop)

        let name = connection.Client.RemoteEndPoint.ToString().Split([|":"|], StringSplitOptions.None).[1]
        sw.WriteLine("INICIO")
        clients.Add(name, sw)
        let rec mainLoop (stream: NetworkStream) (msg: string) = 
            try
                // send received message to all clients
                let msg = Printf.sprintf "%s: %s" name msg
                clients.SendMessage msg
            with _ ->
                // any error reading a message causes client to disconnect
                clients.Remove name
                sw.Close()
            Async.Start (asyncReadTextAndCont stream mainLoop)
        // welcome the new client by printing "What is you name?"
        //let port = connection.Client.RemoteEndPoint.ToString().Split([|":"|], StringSplitOptions.None).[1]
        //let connRemote = connection.Client.RemoteEndPoint.ToString().Split(default(string[]), StringSplitOptions.RemoveEmptyEntries);
        //let port = connRemote[0]
        //let aa = connRemote :> System.Net.IPEndPoint
        //let! port = connection.Client.RemoteEndPoint.Port //((System.Net.IPEndPoint)connection.Client.RemoteEndPoint).Port;
        //sw.WriteLine("What is your name? "); 
        //sw.Flush()
        // start the main loop that handles reading from the client
        
        Async.Start (asyncReadTextAndCont stream mainLoop)
                        *)
    // create a tcp listener to handle incoming requests
    let listener = new TcpListener(IPAddress.Loopback, 4242)

    // main loop that handles all new connections
    let rec handleConnections() =
        // start the listerner
        listener.Start()
        if listener.Pending() then
            // if there are pending connections, handle them
            async { let! connection = listener.AsyncAcceptTcpClient()
                    printfn "New Connection"
                    // use a thread pool thread to handle the new request 
                    ThreadPool.QueueUserWorkItem(fun _ -> handleClient connection) |> ignore
                    // loop 
                    return! handleConnections() }
        else
            // no pending connections, just loop
            Thread.Sleep(1)
            async { return! handleConnections() }
            
    /// allow tot
    member server.Start() = Async.RunSynchronously (handleConnections())

// start the server class
(new Server()).Start()