namespace Nessos.InstallUtils

    module Registry =

        open Microsoft.Win32
        open System

        open Nessos.Reversible

        let private parsePath (path : string) = path.Split('\\') |> Seq.filter ((<>) "") |> Seq.toList
        let private parsePaths (paths : string list) = paths |> List.collect parsePath
        let private takeUntil idx xs =
            let rec aux idx first rest =
                if idx = 0 then List.rev first
                else
                    match rest with
                    | [] -> raise <| new IndexOutOfRangeException()
                    | x :: xs -> aux (idx - 1) (x :: first) xs

            aux idx [] xs

        let private (|ParseHive|_|) (hive : string) =
            match hive.ToLower() with
            | "hkcr" | "hkey_classes_root" -> Some RegistryHive.ClassesRoot
            | "hklm" | "hkey_local_machine" -> Some RegistryHive.LocalMachine
            | "hkcu" | "hkey_current_user" -> Some RegistryHive.CurrentUser
            | "hku"  | "hkey_users" -> Some RegistryHive.Users
            | "hkcc" | "hkey_current_config" -> Some RegistryHive.CurrentConfig
            | "hkey_performance_data" -> Some RegistryHive.PerformanceData
            | "hkey_dyn_data" -> Some RegistryHive.DynData
            | _ -> None

        let private printHive (hive : RegistryHive) =
            match hive with
            | RegistryHive.ClassesRoot -> "HKEY_CLASSES_ROOT"
            | RegistryHive.LocalMachine -> "HKEY_LOCAL_MACHINE"
            | RegistryHive.CurrentUser -> "HKEY_CURRENT_USER"
            | RegistryHive.Users -> "HKEY_USERS"
            | RegistryHive.CurrentConfig -> "HKEY_CURRENT_CONFIG"
            | RegistryHive.PerformanceData -> "HKEY_PERFORMANCE_DATA"
            | RegistryHive.DynData -> "HKEY_DYN_DATA"
            | _ -> invalidArg "hive" "cannot print enumeration"

        type FsRegistryKey = 
            {
                Hive : RegistryHive
                View : RegistryView
                Hierarchy : string list
            }
        with
            member s.Parent = 
                match s.Hierarchy with
                | [] -> None
                | hs -> Some <| { s with Hierarchy = takeUntil (hs.Length - 1) hs}

            member s.GetSubKey(path : string) = { s with Hierarchy = s.Hierarchy @ parsePath path }
            member s.GetSubKey(path : string list) = { s with Hierarchy = s.Hierarchy @ parsePaths path }
            member s.GetField<'T>(?name : string) : FsRegistryField<'T> = { Key = s ; Field = name }

            member k.Path = k.Hierarchy |> String.concat "\\"
            override k.ToString() = sprintf @"%s\%s" <| printHive k.Hive <| k.Path

        and FsRegistryField<'T> =
            {
                Key : FsRegistryKey
                Field : string option
            }
        with
            member v.Value 
                with get () = FsRegistry.GetValue<'T> v
                and set (t : 'T) = FsRegistry.SetValue(v, t)

            override v.ToString() = sprintf @"%O\%s" v.Key <| defaultArg v.Field null

        and FsRegistryField = FsRegistryField<obj>

        and FsRegistry =
        
            static member DefineKey(hive : RegistryHive, view : RegistryView, path : string) =
                match parsePath path with
                | ParseHive _ :: _ -> invalidArg "path" "Key path should not contain hive identifier."
                | path -> { Hive = hive ; View = view ; Hierarchy = path }

            static member DefineKey(hive : RegistryHive, view : RegistryView, path : string list) =
                match parsePaths path with
                | ParseHive _ :: _ -> invalidArg "path" "Key path should not contain hive identifier."
                | _ -> { Hive = hive ; View = view ; Hierarchy = path }

            static member ParseKey(path : string, ?view : RegistryView) =
                let view = defaultArg view RegistryView.Default
                match parsePath path with
                | ParseHive hive :: tail -> { Hive = hive ; View = view ; Hierarchy = tail }
                | _ -> invalidArg "path" "Key missing hive identifier."

            static member ParseKey(path : string list, ?view : RegistryView) =
                let view = defaultArg view RegistryView.Default
                match parsePaths path with
                | ParseHive hive :: tail -> { Hive = hive ; View = view ; Hierarchy = tail }
                | _ -> invalidArg "path" "Key missing hive identifier."

            static member DefineKey(hive : RegistryHive, path : string list, ?view : RegistryView) =
                { Hive = hive ; View = defaultArg view RegistryView.Default ; Hierarchy = parsePaths path }

            static member DefineField<'T> (key : FsRegistryKey, ?name : string) : FsRegistryField<'T> =
                { Key = key ; Field = name }


            // operations

            static member Open(key : FsRegistryKey, ?throwIfMissing, ?createIfMissing, ?writeable) : RegistryKey =
                let writeable = defaultArg writeable false
                let throwIfMissing = defaultArg throwIfMissing false
                let createIfMissing = defaultArg createIfMissing false

                use bkey = RegistryKey.OpenBaseKey(key.Hive, key.View)
                match bkey.OpenSubKey(key.Path, writable = writeable) with
                | null when throwIfMissing -> failwithf "registry key '%O' does not exist." key
                | null when createIfMissing -> bkey.CreateSubKey(key.Path)
                | k -> k

            static member Create(key : FsRegistryKey, ?overwrite) : unit =
                let overwrite = defaultArg overwrite false

                if not overwrite && FsRegistry.Exists key then
                    failwithf "registry key '%O' already exists." key
                else
                    use k = FsRegistry.Open(key, createIfMissing = true) in ()

            static member Delete(key : FsRegistryKey, ?recurse, ?throwIfMissing) =
                let throwIfMissing = defaultArg throwIfMissing true
                let recurse = defaultArg recurse true
                match FsRegistry.Open(key, writeable = true, throwIfMissing = throwIfMissing) with
                | null -> ()
                | rk when recurse -> rk.DeleteSubKeyTree("", true)
                | rk -> rk.DeleteSubKey("", true)

            static member TryGetValueBoxed(key : FsRegistryKey, ?name) =
                let name = defaultArg name null
                use rk = FsRegistry.Open(key)
                match rk with
                | null -> None
                | _ -> denull <| rk.GetValue name

            static member TryGetValue<'T>(key : FsRegistryKey, ?name) : 'T option =
                let name = defaultArg name null
                match FsRegistry.TryGetValueBoxed(key, name) with
                | None -> None
                | Some o ->
                    match o, tryGetParser<'T> with
                    | :? 'T as t, _ -> Some t
                    | (:? string as v), Some p -> Some (p v)
                    | _ ->
                        raise <| InvalidCastException (sprintf @"registry value '%O\%s' is not of type %O" key name typeof<'T>)

            static member GetValue<'T>(key : FsRegistryKey, ?name) : 'T =
                let name = defaultArg name null
                match FsRegistry.TryGetValueBoxed(key, name) with
                | None -> failwithf @"missing registry value '%O\%s'" key name
                | Some o ->
                    match o, tryGetParser<'T> with
                    | :? 'T as t, _ -> t
                    | (:? string as v), Some p -> p v
                    | _ -> 
                        raise <| InvalidCastException (sprintf @"registry value '%O\%s' is not of type %O" key name typeof<'T>)

            static member GetValues(key : FsRegistryKey) : FsRegistryField [] =
                use rk = FsRegistry.Open(key, throwIfMissing = true)
                rk.GetValueNames() |> Array.map (fun name -> { Key = key ; Field = denull name })

            static member GetSubKey(key : FsRegistryKey, name : string) : FsRegistryKey =
                { key with Hierarchy = name :: key.Hierarchy }

            static member GetSubKeys(key : FsRegistryKey) : FsRegistryKey [] =
                use rk = FsRegistry.Open(key, throwIfMissing = true)
                rk.GetSubKeyNames() |> Array.map (fun name -> { key with Hierarchy = name :: key.Hierarchy })

            static member SetValue<'T>(key : FsRegistryKey, value : 'T, ?name, ?overwrite : bool) : unit =
                let name = defaultArg name null
                let overwrite = defaultArg overwrite false
                use rk = FsRegistry.Open(key, createIfMissing = true, writeable = true)
                if not overwrite && FsRegistry.TryGetValueBoxed(key, name).IsSome then
                    failwithf "registry value '%O\%s' already exists." key name
                else
                    rk.SetValue(name, value)

            static member DeleteValue(key : FsRegistryKey, ?name : string, ?throwIfMissing) =
                let name = defaultArg name null
                let throwIfMissing = defaultArg throwIfMissing true
            
                use rk = FsRegistry.Open(key, writeable = true)
                match rk with
                | null -> failwithf "registry value '%O\%s' does not exist." key name
                | _ ->
                    match rk.GetValue(name) with
                    | null when throwIfMissing -> failwithf "registry value '%O\%s' does not exist." key name
                    | null -> ()
                    | _ -> rk.DeleteValue name

            static member Delete(value : FsRegistryField<'T>, ?throwIfMissing) =
                FsRegistry.DeleteValue(value.Key, ?name = value.Field, ?throwIfMissing = throwIfMissing)

            static member TryGetValue<'T>(v : FsRegistryField<'T>) : 'T option =
                FsRegistry.TryGetValue(v.Key, ?name = v.Field)
            static member TryGetValueBoxed<'T>(v : FsRegistryField<'T>) : obj option =
                FsRegistry.TryGetValueBoxed(v.Key, ?name = v.Field)
            static member GetValue<'T>(v : FsRegistryField<'T>) : 'T =
                FsRegistry.GetValue<'T>(v.Key, ?name = v.Field)

            static member SetValue<'T>(v : FsRegistryField<'T>, value : 'T, ?overwrite : bool) : unit =
                FsRegistry.SetValue<'T>(v.Key, value, ?name = v.Field, ?overwrite = overwrite)
            static member SetValueBoxed<'T>(v : FsRegistryField<'T>, value : obj, ?overwrite : bool) : unit =
                FsRegistry.SetValue(v.Key, value, ?name = v.Field, ?overwrite = overwrite)

            static member Exists(key : FsRegistryKey) =
                use k = FsRegistry.Open key in k <> null
            static member Exists<'T>(value : FsRegistryField<'T>) =
                FsRegistry.TryGetValue<'T>(value.Key, ?name = value.Field).IsSome
            static member Exists(key : FsRegistryKey, value) =
                FsRegistry.TryGetValueBoxed(key, value).IsSome

            static member IsValueSet(value : FsRegistryField<bool>) =
                defaultArg (FsRegistry.TryGetValue value) false


            //
            // Reversible primitives for registry
            //

            static member RevCreateKey (key : FsRegistryKey, ?overwrite) =
                let create () = FsRegistry.Create(key, ?overwrite = overwrite), ()
                let recover () = FsRegistry.Delete(key, throwIfMissing = false)
                Reversible.FromComponents(create, recover, ignore)

            static member RevSetValue (regValue : FsRegistryField<'T>, value : 'T, ?overwrite) =
                let setValue () =
                    let previous = FsRegistry.TryGetValueBoxed regValue
                    do FsRegistry.SetValue<'T>(regValue, value, ?overwrite = overwrite)
                    (), previous

                let recover (previous : obj option) =
                    previous |> Option.iter (fun o -> FsRegistry.SetValueBoxed(regValue, o))

                Reversible.FromComponents(setValue, recover, ignore)

            static member RevDeleteValue (regValue : FsRegistryField<'T>, ?throwIfMissing) =
                let deleteValue () =
                    let previous = FsRegistry.TryGetValueBoxed regValue
                    do FsRegistry.Delete(regValue, ?throwIfMissing = throwIfMissing)
                    (), previous

                let recover (previous : obj option) =
                    previous |> Option.iter (fun o -> FsRegistry.SetValueBoxed(regValue, o))

                Reversible.FromComponents(deleteValue, recover, ignore)