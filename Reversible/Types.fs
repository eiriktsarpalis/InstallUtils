namespace Nessos.Reversible

    open System

    type Reversible<'T> = private { Expr : ReversibleExpr }

    and ReversibleResult<'T> =
        | Success of 'T * Reversal // value * finalization
        | Failure of exn * Reversal * Reversal // exn * compensation * finalization
        | Cancelled of Reversal * Reversal // compensation * finalization
        | Fault of exn * Reversal * Reversal // exn * compensation * finalization

    and Reversal =
        | RAtom of Async<unit>
        | RSequential of Reversal list
        | RParallel of Reversal list

    and private Primitive = 
        { 
            Computation : Async<obj * Async<unit> * Async<unit>> // result * recovery * finalization
            Type : System.Type
        }

    and private ReversibleExpr =
        // Syntactic Branches
        | Primitive of Primitive
        | Delay of (unit -> ReversibleExpr)
        | Bind of ReversibleExpr * (obj -> ReversibleExpr)
        | Combine of ReversibleExpr * ReversibleExpr
        | TryWith of ReversibleExpr * (exn -> ReversibleExpr)
        | TryFinally of ReversibleExpr * (unit -> unit)
        | BindDisposable of IDisposable * Type * (obj -> ReversibleExpr)
        | DoSequential of LazyList<ReversibleExpr>
        | RunParallel of Type * ReversibleExpr seq
        // execution specific branches
        | Continuation of (obj -> ReversibleExpr)
        | Combinable of ReversibleExpr
        | WithBlock of (exn -> ReversibleExpr)
        | FinallyBlock of (unit -> unit)
        | Value of obj * Type
        | Exception of exn

    and private TrampolineResult =
        | TValue of obj * Type
        | TException of exn
        | TFault of exn
        | TCancelled

    and ReversibleBuilder() =
        static let lift (f: 'T -> Reversible<'U>) (x : 'S) =
            let expr = f (x :> obj :?> 'T) in expr.Expr

        member __.Delay(f: unit -> Reversible<'T>): Reversible<'T> = { Expr = Delay(lift f) }

        member __.Return(v: 'T): Reversible<'T> =
            let prim = 
                {
                    Computation = async { return v :> obj, async.Zero(), async.Zero() }
                    Type = typeof<'T>
                }
            { Expr = Primitive prim }

        member __.ReturnFrom (f : Reversible<'T>) = f

        member __.Bind(f : Reversible<'T>, cont: 'T -> Reversible<'U>): Reversible<'U> =
            { Expr = Bind(f.Expr, lift cont) }

        member __.Combine(f : Reversible<unit>, g : Reversible<'T>) : Reversible<'T> =
            { Expr = Combine(f.Expr, g.Expr) }

        member __.TryWith(tryBlock: Reversible<'T>, withBlock: exn -> Reversible<'T>): Reversible<'T> =
            { Expr = TryWith(tryBlock.Expr, lift withBlock) }

        member __.TryFinally(tryBlock: Reversible<'T>, finallyF: unit -> unit): Reversible<'T> =
            { Expr = TryFinally(tryBlock.Expr, finallyF) }

        member __.For(inputs : 'T seq, bodyF : 'T -> Reversible<unit>) : Reversible<unit> =
            let expr = DoSequential(inputs |> Seq.map (lift bodyF) |> LazyList.ofSeq)
            { Expr = expr }

        member __.While(pred : unit -> bool, bodyF : Reversible<unit>) : Reversible<unit> =
            let expr = DoSequential(seq { while pred () do yield bodyF.Expr } |> LazyList.ofSeq)
            { Expr = expr }

        member r.Zero() = r.Return()

        member r.Using<'T, 'U when 'T :> IDisposable>(v : 'T, cont : 'T -> Reversible<'U>) : Reversible<'U> =
            { Expr = BindDisposable(v :> IDisposable, typeof<'T>, lift cont) }


    // a few exceptions

    exception CompensationFault of exn
    exception FinalizationFault of exn
    // indicates severe implementation error at the trampoline; recovery workflows are to be run manually
    exception ReversibleInternalFault of exn * Reversal * Reversal  // compensation * finalization

