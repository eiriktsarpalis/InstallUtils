namespace Nessos.InstallUtils

    open System

    open Nessos.Reversible

    module InteractiveConsole =

        type InstallationOption<'T> = 
            {
                Value : 'T
                Description : string
                InitiallyEnabled : bool
            }

        type InstallationOptions =
            static member Prompt (options : InstallationOption<'T> list, ?prompt, ?clearScreen) =
                let prompt = defaultArg prompt "Select the components you would like to install:"

                let clearScreen = defaultArg clearScreen false

                let (|ParseInt|_|) (inp : string) =
                    let n = ref 0
                    if Int32.TryParse(inp, n) then Some n.Value
                    else None

                let print (entries : (InstallationOption<'T> * bool) list) =
                    entries 
                    |> List.iteri (fun i (opt,enabled) ->
                        let ticked = if enabled then "[x]" else "[ ]"
                        printfn "    %s %d. %s" ticked (i+1) opt.Description)

                let rec promptOnce clearScreen (entries : (InstallationOption<'T> * bool) list) =
                    if clearScreen then try Console.Clear() with _ -> ()
                    printfn "%s" prompt
                    print entries
                    printf "\nToggle components (1-%d) (or '*' or 'done') [done] " <| Seq.length entries
                    match Console.ReadLine().Trim().ToLower() with
                    | ""  | "done" -> entries
                    | "*" | "all" -> entries |> List.map (fun (o,_) -> o,true) |> promptOnce clearScreen
                    | ParseInt n ->
                        entries
                        |> List.mapi (fun i (o,e) -> o, if i + 1 = n then not e else e)
                        |> promptOnce clearScreen
                    | _ -> promptOnce clearScreen entries


                options
                |> List.map (fun opt -> opt, opt.InitiallyEnabled)
                |> promptOnce clearScreen
                |> List.choose (fun (opt, enabled) -> if enabled then Some opt else None)


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