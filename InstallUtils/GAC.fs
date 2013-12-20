namespace Nessos.InstallUtils

    open System
    open System.IO
    open System.Reflection

    open Nessos.Reversible

    type GAC private () =
        
        static let publish = System.EnterpriseServices.Internal.Publish()

        /// looks up gac by partial assembly name
        static let lookupByName =
            let gac_paths = lazy(
                let windir = Environment.GetEnvironmentVariable("windir")
                let gacRoot = Path.Combine(windir, @"Microsoft.NET\assembly")
                let paths = [ "GAC_32" ; "GAC_64" ; "GAC_MSIL" ] |> List.map (fun p -> Path.Combine(gacRoot, p))
                paths |> List.filter Directory.Exists)

            let lookup name path =
                let path0 = Path.Combine(path, name)
                if Directory.Exists path0 then
                    Directory.EnumerateFiles(path0, name + ".dll", SearchOption.AllDirectories)
                else Seq.empty

            fun (name : string) ->
                gac_paths.Value
                |> Seq.collect (lookup name)
                |> Seq.map Assembly.ReflectionOnlyLoadFrom 
                |> Seq.toArray

        static let lookupByQualifiedName (aqn : string) =
            aqn.Split(',').[0]
            |> lookupByName
            |> Array.filter (fun a -> a.FullName = aqn)

        static let backupAndRemove (assembly : Assembly) =
            let temp = Path.GetTempPathName ()
            File.Copy(assembly.Location, temp)
            publish.GacRemove assembly.Location
            temp

        static member Install (location : string) = publish.GacInstall location

        static member RevInstall (assemblyName : string, path : string, ?overwrite) =
            let overwrite = defaultArg overwrite false

            let install () =
                if not <| File.Exists path then failwithf "Could not find '%s'." assemblyName

                let assembly = Assembly.ReflectionOnlyLoadFrom path
                let current = lookupByQualifiedName assembly.FullName

                let install, removed =
                    if current.Length > 0 then
                        if overwrite then
                            true, current |> Array.map backupAndRemove
                        else
                            false, [||]
                    else
                        true, [||]

                if install then do publish.GacInstall path
                
                (), (install, removed)

            let recover (installed, removed) = 
                if installed then publish.GacRemove path
                removed |> Array.iter (fun r -> publish.GacInstall r)

            let fini (_,removed) = removed |> Array.iter(fun r -> if File.Exists r then File.Delete r)

            Reversible.FromComponents(install, recover, fini)

        static member RevUninstall (qualifiedName : string, ?throwIfMissing) =
            let throwIfMissing = defaultArg throwIfMissing true

            let uninstall () =
                let previous =
                    qualifiedName
                    |> lookupByQualifiedName
                    |> Array.map(fun a -> 
                        let temp = Path.GetTempPathName ()
                        File.Copy(a.Location, temp)
                        publish.GacRemove a.Location 
                        temp)

                (), previous

            let recover previous =
                previous |> Array.iter (fun path -> publish.GacInstall path)

            let cleanup previous =
                previous |> Array.iter (fun path -> File.Delete path)

            Reversible.FromComponents(uninstall, recover, cleanup)

        static member RevUninstallFrom(location : string, ?throwIfMissing) =
            let a = Assembly.ReflectionOnlyLoadFrom(location)
            GAC.RevUninstall(a.FullName, ?throwIfMissing = throwIfMissing)