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


        [<RequireQualifiedAccess>]
        module List =
            
            let groupBy (f : 'T -> 'S) (xs : 'T list) =
                let map = Seq.groupBy f xs |> Seq.map (fun (x,y) -> x, List.ofSeq y) |> Map.ofSeq
                fun (s : 'S) -> match map.TryFind s with None -> [] | Some xs -> xs

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