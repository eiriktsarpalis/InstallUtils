namespace Nessos.Reversible

    [<AutoOpen>] 
    module Reversible =

        open System
        open System.Threading

        open Nessos.Reversible
        open Nessos.Reversible.Trampoline
        
        let reversible = new ReversibleBuilder ()

        let (|ReversibleFault|_|) = 
            function
            | CompensationFault e -> Some e
            | FinalizationFault e -> Some e
            | ReversibleInternalFault (e,_,_) -> Some e
            | _ -> None

        type Reversible =
            // builder section
            static member FromPrimitiveAsync(computation : Async<'T * _ * _>) : Reversible<'T> = toReversible computation
            
            static member FromAsync(computation : Async<'T>, ?recovery : 'T -> Async<unit>, ?finalization : 'T -> Async<unit>) : Reversible<'T> =
                let recoveryF = defaultArg recovery asyncZero
                let finalizationF = defaultArg finalization asyncZero
                Reversible.FromComponents(computation, recoveryF, finalizationF)
            
            static member FromComponents(computation : Async<'T>, recoveryF : 'T -> Async<unit>, finalizationF: 'T -> Async<unit>) : Reversible<'T> =
                Reversible.FromPrimitiveAsync <| async { let! r = computation in return r, recoveryF r, finalizationF r }
            
            static member FromComponents(computation : Async<'T>, recovery : Async<unit>, finalization : Async<unit>) : Reversible<'T> =
                Reversible.FromPrimitiveAsync <| async { let! r = computation in return r, recovery, finalization }

            static member FromComponents(computation : Async<'T * 'I>, recoveryF : 'I -> Async<unit>, finalizationF : 'I -> Async<unit>) : Reversible<'T> =
                Reversible.FromPrimitiveAsync <| async { let! r, i = computation in return r, recoveryF i, finalizationF i }

            static member FromComponents(computation : unit -> 'T * 'I, recoveryF : 'I -> unit, finalizationF : 'I -> unit) : Reversible<'T> =
                async {
                    let result, innerState = computation ()

                    return result, async { recoveryF innerState }, async { finalizationF innerState }
                } |> Reversible.FromPrimitiveAsync

            static member Parallel(computations : #seq<Reversible<'T>>) = toParallel computations

            static member Ignore(computation : Reversible<'T>) = reversible { let! _ = computation in return () }

            static member Raise(e : #exn) : Reversible<'T> = Reversible.FromAsync <| Async.Raise e

            // execution section

            static member Isolate(computation : Reversible<'T>) = computation |> Reversible.ToAsyncWithRecovery |> Reversible.FromAsync

            static member ToAsyncWithRecovery(computation : Reversible<'T>, ?recoverSequentially, ?recoverOnCancellation, ?cancellationToken : CancellationToken) : Async<'T> =
                let recoverSequentially = defaultArg recoverSequentially false
                let recoverOnCancellation = defaultArg recoverOnCancellation true
                exec recoverSequentially recoverOnCancellation cancellationToken computation
            
            static member RunWithRecovery(computation : Reversible<'T>, ?recoverSequentially, ?timeout, ?cancellationToken : CancellationToken) : 'T =
                let wf = Reversible.ToAsyncWithRecovery(computation, ?recoverSequentially = recoverSequentially, ?cancellationToken = cancellationToken)
                Async.RunSynchronously(wf, ?timeout = timeout)

            static member ToAsyncNoRecovery(computation : Reversible<'T>, ?cancellationToken : CancellationToken) : Async<ReversibleResult<'T>> =
                Async.IsolateCancellation((fun ct -> evalTrampoline ct computation), ?cancellationToken = cancellationToken)

            static member RunNoRecovery(computation : Reversible<'T>, ?cancellationToken, ?timeout) : ReversibleResult<'T> =
                let wf = Reversible.ToAsyncNoRecovery(computation, ?cancellationToken = cancellationToken)
                Async.RunSynchronously(wf, ?timeout = timeout)

            static member RecoverAsync (result : ReversibleResult<'T>, ?runSequentially, ?recoverOnCancellation) : Async<'T> =
                let runSequentially = defaultArg runSequentially false
                let recoverOnCancellation = defaultArg recoverOnCancellation true
                execRecovery runSequentially recoverOnCancellation result

            static member Recover (result : ReversibleResult<'T>, ?runSequentially, ?recoverOnCancellation, ?timeout) =
                let wf = Reversible.RecoverAsync(result, ?runSequentially = runSequentially, ?recoverOnCancellation = recoverOnCancellation)
                Async.RunSynchronously(wf, ?timeout = timeout)


            static member TryRun (computation : Reversible<'T>, ?runSequentially, ?timeout, ?cancellationToken, ?recoverOnCancellation) =
                let recoverOnCancellation = defaultArg recoverOnCancellation true

                let result = Reversible.RunNoRecovery(computation, ?cancellationToken = cancellationToken, 
                                                                                        ?timeout = timeout)
                let mkRecoveryOperation () = async {
                    let! r = Async.Catch <| Reversible.RecoverAsync(result, ?runSequentially = runSequentially, 
                                                                    recoverOnCancellation = recoverOnCancellation)
                    match r with
                    | Choice1Of2 _ -> return ()
                    | Choice2Of2 (ReversibleFault e) -> return! Async.Raise e
                    | Choice2Of2 _ -> return ()
                }

                match result with
                | Success _ -> Reversible.Recover(result, ?runSequentially = runSequentially) |> Choice1Of2
                | Failure(e,_,_)
                | Fault(e,_,_) -> Choice2Of2(e, mkRecoveryOperation ())
                | Cancelled _ ->
                    let recovery = 
                        if recoverOnCancellation then mkRecoveryOperation()
                        else async.Zero()

                    Choice2Of2(OperationCanceledException() :> _, recovery)
