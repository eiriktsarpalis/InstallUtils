namespace Nessos.InstallUtils

    open System
    open System.Diagnostics
    open System.IO
    open System.Threading
    open System.Threading.Tasks
    open System.Security.Principal

    open Microsoft.Win32

    [<AutoOpen>]
    module internal Utils =

        let currentUserIsAdmin () =
            try
                let user = WindowsIdentity.GetCurrent()
                let principal = new WindowsPrincipal(user)
                principal.IsInRole(WindowsBuiltInRole.Administrator)
            with _ -> false

        
        let RunsOnMono =
            match Type.GetType("Mono.Runtime") with
            | null -> false
            | _ -> true

        let inline denull< ^T when ^T : null> (x : 'T) = match x with null -> None | x -> Some x

        [<RequireQualifiedAccess>]
        module Option =
            
            let filter (f : 'T -> bool) =
                function
                | Some x when f x -> Some x
                | _ -> None


        type Microsoft.FSharp.Control.Async with
            static member Raise(e : exn) = Async.FromContinuations(fun (_,ec,_) -> ec e)

            /// untyped awaitTask
            static member AwaitTask (t : Task) = t.ContinueWith ignore |> Async.AwaitTask
            /// non-blocking awaitTask with timeout
            static member AwaitTask (t : Task<'T>, timeout : int) =
                async {
                    use cts = new CancellationTokenSource()
                    use timer = Task.Delay (timeout, cts.Token)
                    try
                        let! completed = Async.AwaitTask <| Task.WhenAny(t, timer)
                        if completed = (t :> Task) then
                            let! result = Async.AwaitTask t
                            return Some result
                        else return None

                    finally cts.Cancel()
                }

        type AsyncResultCell<'T>() =
            let completionSource = new TaskCompletionSource<'T>()

            member c.RegisterResult(result: 'T) = completionSource.SetResult(result)
            member c.AsyncWaitResult(millisecondsTimeout: int): Async<'T option> =
                Async.AwaitTask(completionSource.Task, millisecondsTimeout)

            // use default AwaitTask when no timeout overload is given
            member c.AsyncWaitResult(): Async<'T> =
                Async.AwaitTask(completionSource.Task)

        type Microsoft.FSharp.Control.Async with 
            static member AwaitObservable(observable: IObservable<'T>, ?timeout) =
                let resultCell = new AsyncResultCell<'T>()
                let rec observer = (fun result ->
                    resultCell.RegisterResult(result)
                    remover.Dispose())
                and remover: IDisposable = observable.Subscribe resultCell.RegisterResult

                match timeout with
                | None -> resultCell.AsyncWaitResult()
                | Some t ->
                    async {
                        let! r = resultCell.AsyncWaitResult t
                        
                        match r with
                        | None -> return! Async.Raise <| TimeoutException()
                        | Some v -> return v
                    }


        [<RequireQualifiedAccess>]
        module Process =
            let startAndAwaitTerminationAsync(psi : ProcessStartInfo) =
                async {
                    let proc = new Process()
                    proc.StartInfo <- psi
                    proc.EnableRaisingEvents <- true
                    if proc.Start() then
                        let! _ = Async.AwaitObservable proc.Exited
                        return proc
                    else
                        return failwith "error starting process"
                }

            let startAndAwaitTermination(psi : ProcessStartInfo) =
                startAndAwaitTerminationAsync psi |> Async.RunSynchronously

        /// will only return a mutex if name hasn't been claimed yet
        let tryClaimGlobalMutex (name : string) =
            // @ http://social.msdn.microsoft.com/forums/en-US/netfxbcl/thread/47e6ee95-f2dc-45cd-b456-0e755b99bb52
            let name = @"Global\" + name.Replace('\\', '/')
            let isCreatedNew = ref false
            try
                let mtx = new Mutex(true, name, isCreatedNew)
                if !isCreatedNew then Some mtx
                else mtx.Close () ; None
            with _ -> None


        type Path with
            static member GetTempPathName () =
                Path.Combine(Path.GetTempPath(), Path.GetRandomFileName())


        let private parsers =
            let inline dc (f : string -> 'T) = (typeof<'T>, f :> obj)
            [
                dc System.Boolean.Parse
                dc int32
                dc float
                dc float
                dc int64
                dc int16
                dc uint16
                dc uint32
                dc uint64
                dc sbyte
                dc decimal
                dc System.DateTime.Parse
                dc System.Guid.Parse
                dc id
            ] 
            |> Seq.map(fun (t,p) -> t.MetadataToken, p)
            |> Map.ofSeq

        let tryGetParser<'T> = 
            let inline uc (x : obj) = x :?> 'T
            parsers.TryFind typeof<'T>.MetadataToken
            |> Option.map (fun p -> p :?> string -> 'T)