namespace Nessos.Reversible

    open System
    open System.Diagnostics
    open System.IO
    open System.Threading
    open System.Threading.Tasks
    open System.Security.Principal

    open Microsoft.Win32

    [<AutoOpen>]
    module internal Utils =

        // ad hoc lazylist implementation

        type LazyList<'T> =
            private
            | Empty
            | Tail of Lazy<LazyList<'T>>
            | NonEmpty of 'T * LazyList<'T>
        
        [<RequireQualifiedAccess>]
        module LazyList =
            let rec (|Nil|Cons|) (ll : LazyList<'T>) =
                match ll with
                | Empty -> Nil
                | Tail lt -> (|Nil|Cons|) lt.Value
                | NonEmpty(t,tl) -> Cons(t, tl)
            
            
            let ofSeq (xs : 'T seq) =
                let e = xs.GetEnumerator()
                let rec unfold () = lazy(
                    if e.MoveNext() then
                        NonEmpty(e.Current, Tail(unfold()))
                    else Empty )

                Tail(unfold ())


        let inline denull< ^T when ^T : null> (x : 'T) = match x with null -> None | x -> Some x


        [<RequireQualifiedAccess>]
        module List =
            
            let groupBy (f : 'T -> 'S) (xs : 'T list) =
                let map = Seq.groupBy f xs |> Seq.map (fun (x,y) -> x, List.ofSeq y) |> Map.ofSeq
                fun (s : 'S) -> match map.TryFind s with None -> [] | Some xs -> xs


        [<RequireQualifiedAccess>]
        module Option =
            
            let filter (f : 'T -> bool) =
                function
                | Some x when f x -> Some x
                | _ -> None


        type Microsoft.FSharp.Control.Async with
            static member Raise(e : exn) = Async.FromContinuations(fun (_,ec,_) -> ec e)

            static member IsolateCancellation (computationF : CancellationToken -> Async<'T>, ?cancellationToken : CancellationToken) : Async<'T> =
                async {
                    let! ct = 
                        match cancellationToken with
                        | None -> Async.CancellationToken
                        | Some ct -> async.Return ct

                    try
                        return! Async.AwaitTask <| Async.StartAsTask(computationF ct)
                    with :? AggregateException as e when e.InnerExceptions.Count = 1 ->
                        return! Async.Raise <| e.InnerExceptions.[0]
                }

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