namespace Nessos.Reversible

    module internal Trampoline =

        open System
        open System.Threading

        open Nessos.Reversible

        // trampoline execution logic ; should be exception safe
        let evalTrampoline (ct : CancellationToken) (reversible: Reversible<'T>) : Async<ReversibleResult<'T>> =
            // reversible bind
            let append item workflow =
                match workflow with
                | RAtom _ | RParallel _ -> RSequential([item ; workflow])
                | RSequential items -> RSequential(item :: items)

            let catch f t = try Choice1Of2 (f t) with e -> Choice2Of2 e

            let inline cancel (cts : CancellationTokenSource) = cts.Cancel()

            let inline isCancelled (cts : CancellationTokenSource) = cts.IsCancellationRequested

            let rec eval ((cts, compensation, finalization) as state) code =
                async {
                    // check for external cancellation first
                    if isCancelled cts then return TCancelled, compensation, finalization else

                    match code with
                    // return evaluated result
                    | Value(o,t) :: [] -> return TValue(o,t), compensation, finalization
                    | Exception e :: [] ->
                        do cancel cts
                        return TException e, compensation, finalization

                    // Bind
                    | Bind(f, g) :: rest -> 
                        return! eval state (f :: Continuation g :: rest)
                    | BindDisposable(d, t, f) :: rest ->
                        let disposal = RAtom <| async { do d.Dispose() }
                        return! eval (cts, compensation, append disposal finalization) (Value(d, t) :: Continuation f :: rest)
                    | Value(v,_) :: Continuation f :: rest ->
                        match catch f v with
                        | Choice1Of2 expr -> return! eval state (expr :: rest)
                        | Choice2Of2 e -> return! eval state (Exception e :: rest)

                    // Delay
                    | Delay f :: rest ->
                        match catch f () with
                        | Choice1Of2 expr -> return! eval state (expr :: rest)
                        | Choice2Of2 e -> return! eval state (Exception e :: rest)

                    // Combine
                    | Combine(f, g) :: rest -> return! eval state (f :: Combinable g :: rest)
                    | Value _ :: Combinable g :: rest -> return! eval state (g :: rest)

                    // Sequentials
                    | DoSequential(LazyList.Cons(hd, tl)) :: rest ->
                        let unfolded = Combine(hd, DoSequential tl)
                        return! eval state (unfolded :: rest)
                    | DoSequential LazyList.Nil :: rest ->
                        let vunit = Value((), typeof<unit>)
                        return! eval state (vunit :: rest)

                    // Try/With
                    | TryWith(tryBlock, withF) :: rest ->
                        return! eval state (tryBlock :: WithBlock withF :: rest)
                    | Value _ as v :: WithBlock _ :: rest -> return! eval state (v :: rest)
                    | Exception e :: WithBlock h :: rest ->
                        match catch h e with
                        | Choice1Of2 expr -> return! eval state (expr :: rest)
                        | Choice2Of2 e -> return! eval state (Exception e :: rest)

                    // Try/Finally
                    | TryFinally(tryBlock, finalizationF) :: rest ->
                        return! eval state (tryBlock :: FinallyBlock finalizationF :: rest)
                    | (Value _ | Exception _) as r :: (FinallyBlock _ as f) :: rest -> return! eval state (f :: r :: rest)
                    | FinallyBlock f :: rest ->
                        match catch f () with
                        | Choice1Of2 _ -> return! eval state rest
                        | Choice2Of2 e -> return! eval state (Exception e :: rest)

                    // primitive handling
                    | Primitive { Computation = computation ; Type = t } :: rest ->
                        let! result = Async.Catch computation

                        match result with
                        | Choice1Of2 (value, comp, final) ->
                            let state' = cts, append (RAtom comp) compensation, append (RAtom final) finalization
                            return! eval state' (Value(value, t) :: rest)
                        | Choice2Of2 e -> return! eval state (Exception e :: rest)

                    // Parallel workflows
                    | RunParallel (t, children) :: rest ->
                        use cts' = CancellationTokenSource.CreateLinkedTokenSource [| cts.Token |]

                        let wrap child = eval (cts', RSequential [], RSequential []) [child]
                        let! result = children |> Seq.map wrap |> Async.Parallel

                        let results, compensations, finalizations = result |> Array.toList |> List.unzip3
                        let compensation' = append (RParallel compensations) compensation
                        let finalization' = append (RParallel finalizations) finalization

                        let map = results |> List.groupBy (function TValue _ -> 0 | TException _ -> 1 | TFault _ -> 2 | TCancelled _ -> 3)

                        let values = map 0
                        let exns = map 1
                        let faults = map 2
                        let cancelled = map 3

                        if faults.Length > 0 then
                            do cancel cts
                            return faults.[0], compensation', finalization'
                        elif exns.Length > 0 then
                            let e = match exns.[0] with TException e -> e | _ -> failwith "impossible."
                            return! eval (cts,compensation',finalization') (Exception e :: rest)
                        elif cancelled.Length > 0 then
                            return
                                if isCancelled cts then TCancelled, compensation', finalization'
                                else TFault(new Exception("Reversible parallel inconsistency.")), compensation', finalization'
                        else
                            // computation successful, create result array
                            let result =
                                try
                                    let objValues = 
                                        values 
                                        |> Seq.map (function TValue(o,_) -> o | _ -> failwith "impossible.") 
                                        |> Array.ofSeq
                                    let resultArr = Array.CreateInstance(t, objValues.Length)
                                    do Array.Copy(objValues, resultArr, objValues.Length)
                                    Choice1Of2 (Value(resultArr, resultArr.GetType()))
                                with e -> Choice2Of2 (TFault(new Exception("Reversible parallel inconsistency", e)))

                            match result with
                            | Choice1Of2 value -> return! eval (cts,compensation',finalization') (value :: rest)
                            | Choice2Of2 fault -> return fault, compensation', finalization'

                    // roll exceptions to the top of the stack
                    | Exception _ as e :: _ :: rest -> return! eval state (e :: rest)
                    // handle corrupt interpreter state
                    | stack -> 
                        do cancel cts
                        return TFault(new Exception(sprintf "Trampoline dump: %A" stack)), compensation, finalization
                }

            async {
                let cts = CancellationTokenSource.CreateLinkedTokenSource [| ct |]
                let! result, compensation, finalization = eval (cts, RSequential [], RSequential []) [reversible.Expr]

                match result with
                | TCancelled ->
                    assert ct.IsCancellationRequested
                    return Cancelled(compensation, finalization)
                | TException e -> return Failure(e, compensation, finalization)
                | TFault e -> return Fault(e, compensation, finalization)
                | TValue(o,_) ->
                    try let v = o :?> 'T in return Success(v, finalization)
                    with e -> return Fault(e, compensation, finalization)
            }

        // execute recovery workflow
        let evalRecovery runSeq wrap (r : Reversal) : Async<unit> =
            let rec eval r = async {
                match r with
                | RAtom r -> return! r
                | RSequential rs -> for r in rs do do! eval r
                | RParallel rs when runSeq -> for r in rs do do! eval r
                | RParallel rs -> do! Seq.map eval rs |> Async.Parallel |> Async.Ignore
            }

            async { try return! eval r with e -> return! Async.Raise (wrap e) }

        let execRecovery isRecoverySequential recoverOnCancellation (r : ReversibleResult<'T>) : Async<'T> =
            async {
                match r with
                | Success(result, finalization) ->
                    do! evalRecovery isRecoverySequential FinalizationFault finalization
                    return result
                | Failure(e, compensation, finalization) ->
                    do! evalRecovery isRecoverySequential CompensationFault compensation
                    do! evalRecovery isRecoverySequential FinalizationFault finalization
                    return! Async.Raise e
                | Cancelled(compensation, finalization) ->
                    if recoverOnCancellation then
                        do! evalRecovery isRecoverySequential CompensationFault compensation
                        do! evalRecovery isRecoverySequential FinalizationFault finalization

                    return! Async.Raise(new OperationCanceledException())
                | Fault(e, compensation, finalization) ->
                    return! Async.Raise <| ReversibleInternalFault(e, compensation, finalization)
            }

        let execTrampoline isRecoverySequential recoverOnCancellation ct (r : Reversible<'T>) : Async<'T> =
            async {
                let! result = evalTrampoline ct r

                return! execRecovery isRecoverySequential recoverOnCancellation result
            }

        let exec isRecoverySequential recoverOnCancellation (ct: CancellationToken option) (r : Reversible<'T>) =
            Async.IsolateCancellation((fun ct' -> execTrampoline isRecoverySequential recoverOnCancellation ct' r), ?cancellationToken = ct)

        let toReversible (input : Async<'T * Async<unit> * Async<unit>>) : Reversible<'T> =
            let prim = { Computation = async { let! r,x,y = input in return r :> obj, x, y } ; Type = typeof<'T> }
            { Expr = Primitive prim }

        let toParallel (inputs : Reversible<'T> seq) : Reversible<'T []> =
            { Expr = RunParallel(typeof<'T>, inputs |> Seq.map (fun r -> r.Expr)) }

        /// polymorphic Async.Zero with ignore semantics
        let asyncZero _ = async.Zero()