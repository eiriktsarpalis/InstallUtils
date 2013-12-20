namespace Nessos.Reversible

    [<AutoOpen>]
    module Extensions =

        open System

        [<RequireQualifiedAccess>]
        module Reversible =
            /// postcompose covariant operation
            let map (f : 'T -> 'S) (w : Reversible<'T>) : Reversible<'S> =
                reversible { let! r = w in return f r }

            /// lifting of lambdas to revasync funcs
            let lift (f : 'T -> 'S) = fun t -> reversible { return f t }
        

            /// reversible failwith
            let rfailwith msg = Reversible.Raise(Exception msg) : Reversible<'T>
            /// reversible failwithf
            let rfailwithf fmt = Printf.ksprintf rfailwith fmt : Reversible<'T>

            let rec fold (foldF: 'U -> 'T -> Reversible<'U>) (state: 'U) (items: 'T list): Reversible<'U> =
                reversible {
                    match items with
                    | [] -> return state
                    | item::rest ->
                        let! nextState = foldF state item
                        return! fold foldF nextState rest
                }

            let foldBack (foldF: 'T -> 'U -> Reversible<'U>) (items: 'T list) (state: 'U): Reversible<'U> =
                let rec loop is k = reversible {
                    match is with
                    | [] -> return! k state
                    | h::t -> return! loop t (fun acc -> reversible { let! acc' = foldF h acc in return! k acc' })
                }

                loop items reversible.Return

            let bindMap (mapF: 'T -> Reversible<'U>) (items: 'T list) : Reversible<'U list> =
                fold (fun is i -> reversible { let! i' = mapF i in return i'::is }) [] items

            let choose (choiceF: 'T -> Reversible<'U option>) (items: 'T list): Reversible<'U list> =
                fold (fun is i -> reversible { let! r = choiceF i in return match r with Some i' -> i'::is | _ -> is }) [] items