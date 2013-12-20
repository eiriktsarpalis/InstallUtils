namespace Nessos.InstallUtils

    open System.IO
    open System.Management
    open System.ServiceProcess
    open System.Diagnostics

    open Nessos.Reversible

    type Service private () =

        /// returns path of currently installed service by given name
        static let tryFindInstalledServiceByName (name : string) =
            use mc = new ManagementClass("Win32_Service")
            mc.GetInstances () 
            |> Seq.cast<ManagementBaseObject>
            |> Seq.tryPick (fun mo -> 
                if mo.GetPropertyValue("Name") :?> string = name then
                    let path = mo.GetPropertyValue("PathName").ToString().Trim('"')
                    let running = mo.GetPropertyValue("State").ToString() = "Running"
                    Some(path, running)
                else None)

        static let runInstallUtil (args : string []) =
            let dotNetRoot = System.Runtime.InteropServices.RuntimeEnvironment.GetRuntimeDirectory()
            let util = Path.Combine(dotNetRoot, "installutil.exe")
            let args = args |> Seq.map (fun a -> "\"" + a + "\"") |> String.concat " "

            if not <| File.Exists util then failwith "Could not locate installutil.exe"

            let psi = new ProcessStartInfo(util, args)
            psi.CreateNoWindow <- true
            psi.UseShellExecute <- false
            psi.WorkingDirectory <- Path.GetTempPath()

            let proc = Process.startAndAwaitTermination psi

            if proc.ExitCode = 0 then ()
            else failwith "error running installutil"

        static let installSvc path = runInstallUtil [| path |]
        static let uninstallSvc path = runInstallUtil [| "/u" ; path |]

        static member Start (name : string) =
            use svcc = new ServiceController(name)
            svcc.Start ()

        static member RevInstall (name : string, location : string, ?overwrite) =
            let overwrite = defaultArg overwrite false

            let install () =
                let residentService = tryFindInstalledServiceByName name
                if not <| overwrite && residentService.IsSome then
                    failwith "a service by the name '%s' is already running on this machine." name    
                residentService |> Option.iter (fst >> uninstallSvc)
                installSvc location
                (), residentService

            let recover previous =
                uninstallSvc location
                match previous with
                | Some (path, wasRunning) when File.Exists path -> 
                    installSvc path
                    if wasRunning then
                        use svcc = new ServiceController(name)
                        svcc.Start()
                | _ -> ()
        
            Reversible.FromComponents(install, recover, ignore)


        static member RevUninstall (name : string, ?throwIfMissing) =
            let throwIfMissing = defaultArg throwIfMissing true

            let uninstall () =
                let residentService = tryFindInstalledServiceByName name
                if throwIfMissing && residentService.IsNone then
                    failwith "no service by the name '%s' could be found on this machine." name
                residentService |> Option.iter (fst >> uninstallSvc)
                (), residentService

            let recover previous =
                match previous with
                | Some (path, wasRunning) when File.Exists path -> 
                    installSvc path
                    if wasRunning then
                        use svcc = new ServiceController(name)
                        svcc.Start()
                | _ -> ()

            Reversible.FromComponents(uninstall, recover, ignore)