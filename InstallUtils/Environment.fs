namespace Nessos.InstallUtils

    open System

    open Nessos.Reversible

    type Environment private () =

        static let getPath target = 
            Environment.GetEnvironmentVariable("Path", target).Split(';')

        static let setPath target entries = 
            let path = entries |> String.concat ";"
            async { 
                return Environment.SetEnvironmentVariable("Path", path, target)
            } |> Async.Start

        static member RevIncludePath (path : string, ?target) =
            let target = defaultArg target EnvironmentVariableTarget.Machine
            let set () =
                let entries = getPath target
                if Array.exists ((=) path) entries then (), None
                else
                    setPath target <| Seq.append entries [path]
                    (), Some entries

            let undo = 
                function
                | None -> ()
                | Some entries -> setPath target entries

            Reversible.FromComponents(set, undo, ignore)

        static member RevRemovePath (path : string, ?target) =
            let target = defaultArg target EnvironmentVariableTarget.Machine
            let remove () =
                let entries = getPath target
                if Array.exists ((=) path) entries then
                    setPath target <| Seq.filter ((<>) path) entries
                    (), Some entries

                else (), None

            let undo =
                function
                | None -> ()
                | Some entries -> setPath target entries

            Reversible.FromComponents(remove, undo, ignore)