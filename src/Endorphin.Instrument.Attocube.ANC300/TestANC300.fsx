// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

// Warning: generated file; your changes could be lost when a new file is generated.
#I __SOURCE_DIRECTORY__
#r "../../packages/log4net/lib/net45-full/log4net.dll"
#r "System.Core.dll"
#r "System.dll"
#r "System.Numerics.dll"
#r "../../packages/Endorphin.IO/lib/net452/Endorphin.IO.dll"
#r "./bin/Debug/Endorphin.Instrument.Attocube.ANC300.dll"

open Endorphin.Instrument.Attocube.ANC300
open FSharp.Data.UnitSystems.SI.UnitSymbols

//log4net.Config.BasicConfigurator.Configure()

let readStatus = async {
    use a = new ANC300("attocube")
    printfn "Z mode : %A" (a.X.Mode)
    printfn "Z voltage : %A" (a.X.Voltage)
    printfn "Z frequency : %A" (a.X.Frequency)
    let path = a.X.GenerateOscillation 0.5 10
    path |> List.map (sprintf "%.2f") |> List.reduce (sprintf "%s %s") |> printfn "V: %s"
    Async.Start <| a.Z.Path path 1.0<s> false
    for i in [1 .. 10] do
        printfn "V: %.2f" a.Z.Offset
        do! Async.Sleep 50

    let A = [1..308]
    A |> a.Lua.SetArray "test"
    let i = a.Lua.Query "for i=1,#test do print(test[i]) end"
    i |> List.iter (printfn "%s")
    a.Lua.Query "print(#test)" |> List.map (printfn "%s") |> ignore
    printfn "i.Length %d %d" A.Length i.Length
    }


Async.RunSynchronously readStatus


