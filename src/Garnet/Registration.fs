namespace Garnet.Composition

open System
open System.Collections.Generic
open Garnet.Comparisons

[<Struct>]
type Registration = {
    registeredName : string
    register : Container -> IDisposable
    }

type RegistrationLookup() =
    let dict = Dictionary<_, IDisposable>()
    member c.Unregister name sub =
        match dict.TryGetValue name with
        | false, _ -> ()            
        | true, x -> 
            if obj.ReferenceEquals(x, sub) then 
                x.Dispose()
                dict.Remove name |> ignore
    member c.UnregisterAny name =
        match dict.TryGetValue name with
        | false, _ -> ()            
        | true, x -> 
            x.Dispose()
            dict.Remove name |> ignore
    member c.Register name disposable =
        c.UnregisterAny name
        dict.[name] <- disposable
    override c.ToString() =
        sprintf "Systems (%d)" dict.Count + 
            (if dict.Count > 0 then ":\n  " + String.Join("\n  ", dict.Keys) else "")

module Registration =
    /// Creates a hotswappable system from a registration function
    let named name register = {
        registeredName = name
        register = register 
        }
        
    /// Creates a hotswappable system from a list of registration functions
    let listNamed name registrations =
        named name <| Disposable.combine registrations
        
    /// Creates a system from a registration function
    let init register = named "" register

    /// Creates a system from a list of registration functions
    let list registrations = listNamed "" registrations

    /// Registers a system with container, hotswappable if name defined
    let register system c =
        if String.IsNullOrEmpty system.registeredName 
        then system.register c
        else
            // unregister any existing with name
            let lookup = c.GetInstance<RegistrationLookup>()
            lookup.UnregisterAny system.registeredName
            // create new registration
            let sub = system.register c
            lookup.Register system.registeredName sub
            // return a disposable that unregisters
            Disposable.init <| fun () -> 
                lookup.Unregister system.registeredName sub
        
    let registerTo c system =
        register system c

    /// Makes a system hotswappable by wrapping in a named system
    /// Note original (possibly named) system is still present
    let withName named name def =
        register def |> named name

    /// Creates a hotswappable system from a list of systems
    let combineNamed name defs =
        // map definitions to registration calls
        defs 
        |> List.map register
        |> listNamed name

    /// Creates a system from a list of systems
    let combine defs = combineNamed "" defs

    let toContainer system =
        let c = Container()
        register system c |> ignore
        c
