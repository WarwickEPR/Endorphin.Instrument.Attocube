// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

// Warning: generated file; your changes could be lost when a new file is generated.
#I __SOURCE_DIRECTORY__
#r "../../packages/log4net/lib/net45-full/log4net.dll"
#r "System.Core.dll"
#r "System.dll"
#r "System.Numerics.dll"
#r "./bin/Debug/Endorphin.Instrument.Attocube.ANC300.dll"

open Endorphin.Instrument.Attocube.ANC300
open System

let readStatus = async {
    use a = new ANC300("attocube")
    printfn "Z mode : %A" (a.X().Mode)
    printfn "Z voltage : %A" (a.X().Voltage)
    printfn "Z frequency : %A" (a.X().Frequency)
    }

Async.RunSynchronously readStatus


