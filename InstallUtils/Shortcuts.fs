namespace Nessos.InstallUtils

    open System
    open System.IO

    open Nessos.Reversible

    type ShortcutDescriptor =
        {
            Name : string
            SourceFile : string
            TargetDir : string
            Icon : string option
        }
    with
        member d.Path = Path.Combine(d.TargetDir, d.Name + ".url") |> Path.GetFullPath

        static member Create (d : ShortcutDescriptor) : unit =
            if File.Exists d.Path then File.Delete d.Path
            elif not <| Directory.Exists d.TargetDir then 
                Directory.CreateDirectory d.TargetDir |> ignore

            use sw = new StreamWriter(d.Path)
            sw.WriteLine("[InternetShortcut]")
            sw.WriteLine("URL=file:///" + d.SourceFile)
            d.Icon |> Option.iter (fun icon ->
                sw.WriteLine("IconIndex=0")
                sw.WriteLine("IconFile=" + icon.Replace('\\','/')))

            sw.Flush()

        static member OfSpecialFolder(source : string, targetFolder : Environment.SpecialFolder, name : string, ?icon, ?subDir) =
            let icon = defaultArg icon source
            let targetDir = Path.Combine(Environment.GetFolderPath targetFolder, defaultArg subDir "")
            { Name = name ; SourceFile = source ; TargetDir = targetDir ; Icon = Some icon }

        static member RevCreate (d : ShortcutDescriptor) =
            let create () =
                let backup = 
                    if File.Exists d.Path then
                        let backup = Path.Combine(Path.GetTempPath(), Path.GetRandomFileName())
                        File.Move(d.Path, backup); Some backup
                    else None

                ShortcutDescriptor.Create d

                (), backup

            let recover backup =
                File.Delete d.Path
                backup |> Option.iter (fun b -> File.Move(b, d.Path))

            let fini backup = backup |> Option.iter (fun b -> if File.Exists b then File.Delete b)

            Reversible.FromComponents(create, recover, fini)

        static member RevDelete (d : ShortcutDescriptor) = File.RevDelete(d.Path, false)
