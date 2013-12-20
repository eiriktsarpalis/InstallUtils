namespace Nessos.InstallUtils

    open System

    open NetFwTypeLib

    open Nessos.Reversible

    type Firewall private () =

        static let getManager () =
            let t = Type.GetTypeFromCLSID(Guid("{304CE942-6E39-40D8-943A-B913C40C9CD4}"))
            Activator.CreateInstance(t) :?> INetFwMgr

        static let addException (name : string) (path : string) =
            let mgr = getManager ()
            let t = Type.GetTypeFromProgID("HNetCfg.FwAuthorizedApplication")
            let appEntry = Activator.CreateInstance(t) :?> INetFwAuthorizedApplication

            appEntry.Name <- name
            appEntry.ProcessImageFileName <- path
            appEntry.Scope <- NET_FW_SCOPE_.NET_FW_SCOPE_ALL
            appEntry.IpVersion <- NET_FW_IP_VERSION_.NET_FW_IP_VERSION_ANY
            appEntry.Enabled <- true

            mgr.LocalPolicy.CurrentProfile.AuthorizedApplications.Add appEntry

        static let removeException (path : string) =   
            let mgr = getManager ()
            mgr.LocalPolicy.CurrentProfile.AuthorizedApplications.Remove path

        static let tryFindRule (f : INetFwAuthorizedApplication -> bool) =
            getManager().LocalPolicy.CurrentProfile.AuthorizedApplications
            |> Seq.cast<INetFwAuthorizedApplication>
            |> Seq.tryFind f

        static member RevAuthorize (ruleName : string, servicePath : string, ?overwrite) =
            let overwrite = defaultArg overwrite false
            let authorize () = 
                let currentRule = tryFindRule (fun r -> r.Name = ruleName)
                if not overwrite && currentRule.IsSome then
                    failwithf "a firewall exception by the name '%s' already exists." ruleName
                currentRule |> Option.iter (fun r -> removeException r.ProcessImageFileName)
                addException ruleName servicePath
                (), currentRule |> Option.map (fun r -> r.ProcessImageFileName)

            let recover (previous : string option) = 
                do removeException ruleName
                previous |> Option.iter (addException ruleName)

            Reversible.FromComponents(authorize, recover, ignore)


        static member RevDeauthorize (path : string, ?throwIfMissing) =
            let throwIfMissing = defaultArg throwIfMissing true
            let deauthorize () =
                let current = tryFindRule (fun r -> r.ProcessImageFileName = path)
                if throwIfMissing && current.IsNone then
                    failwith "no firewall exception for the given process image exists."
                current |> Option.iter (fun _ -> removeException path)
                (), current |> Option.map (fun c -> c.Name)

            let recover (previous : string option) =
                previous |> Option.iter (fun name -> addException name path)

            Reversible.FromComponents(deauthorize, recover, ignore)