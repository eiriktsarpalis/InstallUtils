namespace Nessos.InstallUtils

    open IWshRuntimeLibrary

    open Nessos.Reversible

    open System
    open System.IO

    type ShortcutDescriptor =
        {
            Name : string
            Location : string
            IconLocation : string option
            Description : string option

            TargetPath : string
            Arguments : string option
            WorkingDirectory : string option
        }
    with
        member d.Path = Path.Combine(d.Location, d.Name + ".lnk")

        static member Create(name : string, location : string, target : string, ?iconLocation, ?description, ?arguments, ?workingDirectory) =
            {
                Name = name
                Location = location
                TargetPath = target
                IconLocation = iconLocation
                Description = description
                Arguments = arguments
                WorkingDirectory = workingDirectory    
            }

        static member Create(name : string, location, target : string, ?iconLocation, ?description, ?arguments, ?workingDirectory) =
            {
                Name = name
                Location = Environment.GetFolderPath location
                TargetPath = target
                IconLocation = iconLocation
                Description = description
                Arguments = arguments
                WorkingDirectory = workingDirectory    
            }

        static member Save(d : ShortcutDescriptor) =
            let wsh = new WshShellClass()
            let shortcut = wsh.CreateShortcut(d.Path) :?> IWshShortcut
            shortcut.TargetPath <- d.TargetPath
            d.Arguments |> Option.iter(fun arg -> shortcut.Arguments <- arg)
            d.Description |> Option.iter(fun d -> shortcut.Description <- d)
            d.WorkingDirectory |> Option.iter(fun d -> shortcut.WorkingDirectory <- d)
            d.IconLocation |> Option.iter(fun l -> shortcut.IconLocation <- l)
            shortcut.WindowStyle <- 1
            shortcut.Save()

        static member Delete(d : ShortcutDescriptor) = System.IO.File.Delete(d.Path)

        static member RevCreate (d : ShortcutDescriptor) =
            let create () =
                let backup = 
                    if File.Exists d.Path then
                        let backup = Path.Combine(Path.GetTempPath(), Path.GetRandomFileName())
                        File.Move(d.Path, backup); Some backup
                    else None

                ShortcutDescriptor.Save d

                (), backup

            let recover backup =
                File.Delete d.Path
                backup |> Option.iter (fun b -> File.Move(b, d.Path))

            let fini backup = backup |> Option.iter (fun b -> if File.Exists b then File.Delete b)

            Reversible.FromComponents(create, recover, fini)

        static member RevDelete (d : ShortcutDescriptor) = File.RevDelete(d.Path, false)
