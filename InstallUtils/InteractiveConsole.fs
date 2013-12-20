namespace Nessos.InstallUtils

    open System

    open Nessos.Reversible

    module InteractiveConsole =

        type InstallationOption = 
            {
                Description : string
                InitiallyEnabled : bool
            }
        with
            static member Prompt(options : InstallationOption list, ?clearScreen) =
                let clearScreen = defaultArg clearScreen false

                let (|ParseInt|_|) (inp : string) =
                    let n = ref 0
                    if Int32.TryParse(inp, n) then Some n.Value
                    else None

                let print (entries : (InstallationOption * bool) list) =
                    entries 
                    |> List.iteri (fun i (opt,enabled) ->
                        let ticked = if enabled then "[x]" else "[ ]"
                        printfn "    %s %d. %s" ticked (i+1) opt.Description)

                let rec display clearScreen (entries : (InstallationOption * bool) list) =
                    if clearScreen then try Console.Clear() with _ -> ()
                    printfn "Select the components you would like to install: \n"
                    print entries
                    printf "\nEnable/disable components (1-%d) (or '*' or 'done') [done] " <| Seq.length entries
                    match Console.ReadLine().Trim().ToLower() with
                    | ""  | "done" -> entries
                    | "*" | "all" -> entries |> List.map (fun (o,_) -> o,true) |> display clearScreen
                    | ParseInt n ->
                        entries
                        |> List.mapi (fun i (o,e) -> o, if i + 1 = n then not e else e)
                        |> display clearScreen
                    | _ -> display clearScreen entries

                let results =
                    options 
                    |> List.map (fun opt -> opt, opt.InitiallyEnabled)
                    |> display clearScreen
                    |> Map.ofList

                results.TryFind >> Option.exists id


        let readSetting (message : string) (defSetting : string) =
            printf "%s [%s] " message defSetting
            match Console.ReadLine().Trim() with
            | "" -> defSetting
            | results -> results

        let rec areYouSure (message : string) =
            printf "%s [y/N] " message
            match Console.ReadLine().Trim().ToLower() with
            | "y" | "yes" -> true
            | "" | "n" | "no" -> false
            | _ -> printfn "" ; areYouSure message