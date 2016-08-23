// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

namespace Endorphin.Instrument.Attocube

open System
open Endorphin.IO
open FSharp.Data.UnitSystems.SI.UnitSymbols
open System.Text.RegularExpressions

[<AutoOpen>]
module ANC300 =


    type Axis =
        | X
        | Y
        | Z
    let axisNumber = function | X -> 1 | Y -> 2 | Z -> 3

    type Direction = Up | Down

    type PositionerMode =
    | Ground
    | Step
    | OffsetVoltage
     override x.ToString() = match x with
                             | Ground -> "GND"
                             | OffsetVoltage -> "OSV"
                             | Step -> "STP"

    type PositionerState =
    | Off
    | Parked
    | Floating of offset : float<V>

    let private uc (x:string) = x.ToUpper()

    let parseMode = uc >> function
                          | "GND" -> Ground
                          | "STP" -> Step
                          | "OSV" -> OffsetVoltage
                          | x -> failwithf "Unexpected positioner mode: \"%s\"" x

    let luaFunctions =
        [ "require \"io\""
          "require \"os\""
          "function trigger() \
                 local f = io.open(\"/dev/ttyS2\",\"w\"); \
                 f:write(\" \"); \
                 f:close(); \
                 end" /// Cause DTR line on serial port to go high, triggering acquisition (via electronics)
          """function dir(d) for i in io.popen("ls " .. d):lines() do print(i) end end"""
          """function run(x) for i in io.popen(x):lines() do print(i) end end"""
          """function busywait(t) local n = math.ceil(t / 1.42e-3); for i = 0,n do end end""" // in ms
          """function setOffset(axis,v) axis.mode = OSV; axis.osv=v end"""
          "function path(axis,points,dwell,sendTrigger) \
                axis.mode = OSV; \
                for i,v in ipairs(points) do \
                   axis.osv = v \
                   if sendTrigger then trigger() end \
                   busywait(dwell) \
                end \
                if sendTrigger then trigger() end \
             end"
          "function sweep(axis,start,finish,stepsize,dwell,sendTrigger) \
                axis.mode = OSV; \
                for v = start,finish,stepsize do axis.osv=v; \
                   if sendTrigger then trigger() end \
                   busywait(dwell); \
                end \
             end"
          "function wobble(axis,start,max,min,stepsize,dwell,sendTrigger) \
               sweep(axis,start,max,stepsize,dwell,sendTrigger) \
               sweep(axis,max-stepsize,min,-stepsize,dwell,sendTrigger) \
               sweep(axis,min+stepsize,start,stepsize,dwell,sendTrigger)" ]

    type Response =
    | Success of string list
    | Failure of string list

    let failOnFailure msg =
        function
        | Success reply -> reply
        | Failure reply ->
            let replyStr = List.reduce (fun a b -> a + "; " + b) reply
            failwithf "%s: %s" msg replyStr

    /// Access to the experimental Lua console
    /// Preferrably use the standard control port rather than build code on the fly
    /// but some functions are only available via Lua, in particular OSV
    type LuaPort(hostname) as this =
        inherit PromptTcpipInstrument("ANC300 Lua port","> ",hostname,7231)

        let luaAxis = function
        | Axis.X -> "m1"
        | Axis.Y -> "m2"
        | Axis.Z -> "m3"

        do
            this.Start()
            "123456" |> this.Query |> ignore
            luaFunctions |> List.iter (this.Query >> ignore)
        
        // expose loaded functions as members

        let parseResponse (response: string list) =
            let response' = response.[1..response.Length-2]
            match response' with
            | error :: tail when error.StartsWith("ERROR") -> failwithf "Lua command failed: %s" error
            | x -> x

        member __.SendCommandSync command =
            let response = command |> this.Query
            response |> parseResponse

        member __.SendCommandAsync command = async {
            let! response = command |> this.QueryAsync
            return response |> parseResponse }

        member x.Oscillate axis start max min stepsize (dwellTime:float<s>) (trigger:bool) =
            (sprintf "wobble(%s,%f,%f,%f,%f,%f,%A)" (luaAxis axis) start max min stepsize dwellTime trigger) |> x.SendCommandAsync |> Async.Ignore

        member x.Path axis (points:float<V> list) (dwell:float<s>) trigger =
            // dwell time per point in seconds, converted to ms
            let points' = List.map (float >> sprintf "%.2f") points
            let points'' = "{ " + (List.reduce (sprintf "%s, %s") points') + " }"
            let command = sprintf "path( %s, %s, %f, %A )" (luaAxis axis) points'' (1000.0</s>*dwell) trigger
            command |> x.SendCommandAsync |> Async.Ignore

        member x.SetMode axis (mode:PositionerMode) =
            sprintf "%s.mode = %A" (luaAxis axis) mode |> x.SendCommandSync |> ignore

        member x.SetOffset axis (offset:float<V>) =
            sprintf "setOffset(%s,%f)" (luaAxis axis) offset |> x.SendCommandSync |> ignore

        member x.Stop axis =
            sprintf "%s:stop()" (luaAxis axis) |> x.SendCommandSync |> ignore



    /// Directly issue commands to the ANC300
    type ControlPort(hostname) as this =
        inherit TcpipInstrument<Response>("ANC300 Control",hostname,7230)

        let extractMode lines =
            let r = new Regex(@"^mode\s*=\s*(\S+)")
            match lines with
            | [response] ->
                let m = r.Match(response)
                if m.Success then
                    m.Groups.[1].Value |> parseMode |> Some
                else
                    None
            | _ -> None

        let extractFrequency lines =
            let r = new Regex(@"^frequency\s*=\s*(\d+)")
            match lines with
            | [response] ->
                let m = r.Match(response)
                if m.Success then
                    m.Groups.[1].Value
                    |> System.Int32.Parse
                    |> LanguagePrimitives.Int32WithMeasure<Hz>
                    |> Some
                else
                    None
            | _ -> None

        let extractVoltage lines =
            let r = new Regex(@"^voltage\s*=\s*(\d+\.\d+)")
            match lines with
            | [response] ->
                let m = r.Match(response)
                if m.Success then
                    m.Groups.[1].Value
                    |> System.Double.Parse
                    |> LanguagePrimitives.FloatWithMeasure<V>
                    |> Some
                else
                    None
            | _ -> None
        
        do
            this.Start()
            "123456" |> this.Send |> ignore
            "echo off" |> this.Query |> ignore

        override __.ExtractReply received =
            let (lines,remainder) = received
            match List.tryFindIndex (function | "OK" | "ERROR" -> true | _ -> false) lines with
            | None -> (received,None)
            | Some i ->
                let (replyLines,lines') = List.splitAt i lines
                let received' = (lines'.Tail,remainder)
                let reply = match lines.[i] with
                            | "OK" -> Success replyLines
                            | _    -> Failure replyLines
                (received',Some reply)
        
        member x.GetMode (axis:Axis) =
            let response = sprintf "getm %d" (axisNumber axis) |> x.Query |> failOnFailure "GetMode failed"
            match extractMode response with
            | Some mode -> mode
            | None -> failwithf "Failed to extract mode"

        member x.SetMode (axis:Axis) (mode:PositionerMode) =
            // Doesn't work for OSV!
            sprintf "setm %d %A" (axisNumber axis) mode |> x.Query |> failOnFailure "SetMode failed" |> ignore

// Not permitted
//        member x.SetOffsetVoltage (axis:Axis) (osv:float<V>) =
//            sprintf "seta %d %f" (axisNumber axis ) osv |> x.SendCommandSync

        member x.GetOffsetVoltage (axis:Axis) =
            let response = sprintf "geta %d" (axisNumber axis) |> x.Query |> failOnFailure "GetOffsetVoltage failed"
            match extractVoltage response with
            | Some voltage -> voltage
            | None -> failwithf "Failed to get offset voltage"

        member x.SetStepVoltage (axis:Axis) (v:float<V>) =
            sprintf "setv %d %f" (axisNumber axis ) v |> x.Query |> failOnFailure "SetStepVoltage failed" |> ignore

        member x.GetStepVoltage (axis:Axis) =
            let response = sprintf "getv %d" (axisNumber axis ) |> x.Query |> failOnFailure "GetStepVoltage failed"
            match extractVoltage response with
            | Some voltage -> voltage
            | None -> failwithf "Failed to get step voltage"

        member x.SetStepFrequency (axis:Axis) (f:int<Hz>) =
            sprintf "setf %d %d" (axisNumber axis ) f |> x.Query |> failOnFailure "SetStepFrequency failed" |> ignore

        member x.GetStepFrequency (axis:Axis) =
            let response = sprintf "getf %d" (axisNumber axis ) |> x.Query |> failOnFailure "GetStepFrequency failed"
            match response |> extractFrequency with
            | Some f -> f
            | None -> failwithf "Failed to get frequency"

        // Can fail if in the wrong mode. Runs in the background on device
        member x.Step (axis:Axis) direction count =
            let command = match direction with Up -> "stepu" | Down -> "stepd"
            async {
                let! response = sprintf "%s %d %d" command (axisNumber axis ) count |> x.QueryAsync
                response |> failOnFailure "Step failed" |> ignore }

        member x.Wait axis = async {
            let! response = sprintf "stepw %d" (axisNumber axis ) |> x.QueryAsync
            response |> failOnFailure "Wait failed" |> ignore }

        member x.Stop (axis:Axis) = 
            axis |> axisNumber |> sprintf "stop %d" |> x.Query |> failOnFailure "Stop failed" |> ignore


    /// Wraps up access to positioners in one interface
    /// Adds checks and compound commands
    type Positioner(hostname,axis) as this =

        let control = new ControlPort(hostname)
        let lua = new LuaPort(hostname)

        let oscillation (centre:float<V>) amplitude points =
            let voltageLimit = this.VoltageLimit()
            let voltageAmplitude = amplitude * voltageLimit
            let max   = if (centre + voltageAmplitude) <= voltageLimit then centre + voltageAmplitude else voltageLimit
            let min   = if (centre - voltageAmplitude) >= 0.0<V> then centre - voltageAmplitude else 0.0<V>
            let stepsize = (max - min) / (float <| points-1)
            [ centre .. stepsize .. max ] @ [ max - stepsize .. - stepsize .. min ] @ [ min + stepsize .. stepsize .. centre ]

        member __.Mode with get() = control.GetMode axis
                        and set mode = lua.SetMode axis mode

        abstract member VoltageLimit : unit -> float<V>
        default __.VoltageLimit() = 60.0<V>

        member x.MidRange() = x.VoltageLimit() * 0.5

        abstract member FrequencyLimit : unit -> int<Hz>
        default __.FrequencyLimit() = 2000<Hz>

        member x.Offset with get() = control.GetOffsetVoltage axis
                         and set v = if v > 0.0<V> && v < x.VoltageLimit() then
                                         lua.SetOffset axis v
                                     else failwithf "Offset %fV out of range" v

        member x.Frequency with get() = control.GetStepFrequency axis
                            and set f = if f > 0<Hz> && f < x.FrequencyLimit() then
                                         control.SetStepFrequency axis f 
                                        else failwithf "Step frequency %dHz out of range" f

        member x.Voltage with get() = control.GetStepVoltage axis
                          and set v = if v > 0.0<V> && v < x.VoltageLimit() then
                                         control.SetStepVoltage axis v
                                      else failwithf "Step voltage %fV out of range" v

        member x.State
            with get() = match x.Mode with
                         | Ground -> Off
                         | Step -> Parked
                         | OffsetVoltage -> Floating x.Offset
            and  set state = match state with
                             | Off -> x.Mode <- Ground
                             | Parked -> x.Mode <- Step
                             | Floating osv ->
                                x.Mode <- OffsetVoltage
                                x.Offset <- osv

        member x.Step direction count = async {
            let step = control.Step axis direction count
            match x.State with
            | Off -> failwithf "Not stepping as positioner %A is in grounded" axis
            | Parked -> do! step
            | Floating osv ->
                x.State <- Parked
                do! step
                x.State <- Floating osv }

        /// Single triangular oscillation of the positioner
        member x.Oscillate depth points period trigger = async {
            let oscillate (centre:float<V>) amplitude points (period:float<s>) trigger =
                let start = centre
                let voltageAmplitude = amplitude * x.VoltageLimit()
                let dwell = period / (float (2 * points))
                let max   = if (centre + voltageAmplitude) <= x.VoltageLimit() then centre + voltageAmplitude else x.VoltageLimit()
                let min   = if (centre + voltageAmplitude) >= 0.0<V> then centre + voltageAmplitude else x.VoltageLimit()
                let stepsize = (max - min) / (float <| points-1)
                lua.Oscillate axis start max min stepsize dwell trigger
            match x.State with
            | Off -> failwithf "Not moving as positioner %A is grounded" axis
            | Parked ->
                x.State <- Floating <| x.MidRange()
                do! oscillate (x.MidRange()) depth points period trigger
                x.State <- Parked
            | Floating osv ->
                do! oscillate osv depth points period trigger }
        
        member x.GenerateOscillation amplitude points =
            match x.State with
            | Parked ->
                oscillation (x.MidRange()) amplitude points
            | Floating osv ->
                oscillation osv amplitude points
            | _ -> []

        member x.Path points (period:float<s>) trigger = async {
            let dwell = period / ( float <| List.length points)
            do! lua.Path axis points dwell trigger }


        member __.Wait() = control.Wait axis
        member __.Stop() = control.Stop axis

        // Starts step operation then waits until it completes
        member x.StepAndWait direction count = async {
            do! x.Step direction count
            do! x.Wait() }

        interface IDisposable with
            member __.Dispose() =
                (control :> IDisposable).Dispose()
                (lua :> IDisposable).Dispose()

    type ANC300(hostname) =

        // To be sure we always have a channel to send stop commands to
        let control = new ControlPort(hostname)
        let xPositioner = new Positioner(hostname,Axis.X)
        let yPositioner = new Positioner(hostname,Axis.Y)
        let zPositioner = new Positioner(hostname,Axis.Z)

        // Each positioner opens its own sockets for control & lua
        member __.X = xPositioner
        member __.Y = yPositioner
        member __.Z = zPositioner
        member __.Axis axis = match axis with
                              | Axis.X -> xPositioner
                              | Axis.Y -> yPositioner
                              | Axis.Z -> zPositioner

        member __.Stop(axis) =
            control.Stop(axis)

        member x.StopAll() =
            x.Stop(Axis.X)
            x.Stop(Axis.Y)
            x.Stop(Axis.Z)

        interface IDisposable with
            member __.Dispose() =
                let dispose x = (x :> IDisposable).Dispose()
                dispose control
                dispose xPositioner
                dispose yPositioner
                dispose zPositioner
