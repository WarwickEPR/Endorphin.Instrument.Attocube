// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

namespace Endorphin.Instrument.Attocube

open System
open Endorphin.IO
open FSharp.Data.UnitSystems.SI.UnitSymbols
open System.Text.RegularExpressions

[<AutoOpen>]
module ANC300 =


    type Axis =
        | X = 1
        | Y = 2
        | Z = 3

    type Direction = Up | Down

    type PositionerMode =
    | OffsetVoltage
    | Step
     override x.ToString() = match x with
                             | OffsetVoltage -> "OSV"
                             | Step -> "STP"

    type PositionerState =
    | Parked
    | Floating of offset : float<V>

    let private uc (x:string) = x.ToUpper()

    let parseMode = uc >> function
                          | "OSV" -> OffsetVoltage
                          | "STP" -> Step
                          | x -> failwithf "Unexpected positioner mode: \"%s\"" x

    let luaFunctions =
        [ "require \"io\""
          "require \"os\""
          """function trigger() \
                 local f = io.open("/dev/ttyS2","w"); \
                 f:write("\n"); \
                 f:close(); \
                 end""" /// Cause DTR line on serial port to go high, triggering acquisition (via electronics)
          """function dir(d) for i in io.popen("ls " .. d):lines() do print(i) end end"""
          """function run(x) for i in io.popen(x):lines() do print(i) end; end"""
          """function busywait(t) local n = math.ceil(t / 1.42e-3); for i = 0,n do end end""" // in ms
          """function setXOffset(v) m1.mode = OSV; m1.osv=v"""
          """function setYOffset(v) m2.mode = OSV; m2.osv=v"""
          """function setZOffset(v) m3.mode = OSV; m3.osv=v"""
          """function pathZ(points,dwell,sendTrigger) \
                m3.mode = OSV; \
                for i,v ipairs do \
                   m3.osv = v \
                   if sendTrigger then trigger() end \
                   busywait(dwell) \
                end
             end"""
          """function sweepZ(start,finish,stepsize,dwell,sendTrigger) \
                m3.mode = OSV;
                for v = start,finish,stepsize do m3.osv=v; \
                   if sendTrigger then trigger() end \
                   busywait(dwell); \
                end \
             end"""
          """function wobbleZ(start,max,min,stepsize,dwell,sendTrigger) \
               sweepZ(start,max,stepsize,dwell,sendTrigger) \
               sweepZ(max-stepsize,min,-stepsize,dwell,sendTrigger) \
               sweepZ(min+stepsize,start,stepsize,dwell,sendTrigger)""" ]


    /// Access to the experimental Lua console
    /// Preferrably use the standard control port rather than build code on the fly
    /// but some functions are only available via Lua, in particular OSV
    type LuaPort(hostname) =

        let logger = log4net.LogManager.GetLogger("ANC300 Lua")
        let handleOutput =
            sprintf "Returned: %s" >> logger.Debug

        let luaPort = new TcpipInstrument("ANC300 Lua port",handleOutput,hostname,1027)

        let luaAxis = function
        | Axis.X -> "m1"
        | Axis.Y -> "m2"
        | Axis.Z -> "m3"
        | x -> failwithf "Invalid axis %A" x

        do
            "123456" |> luaPort.WriteLine
            luaFunctions |> List.iter luaPort.WriteLine
            luaPort.Start()
        
        // expose loaded functions as members

        member __.SendCommandSync command =
            let response = command |> luaPort.QueryUntil (fun x -> x.StartsWith "> ")
            match response with
            | ["OK"] -> ()
            | _ -> failwithf "Lua command failed: %A" response

        member __.SendCommandAsync command = async {
            let! response = command |> luaPort.QueryUntilAsync (fun x -> x.StartsWith "> ")
            match response with
            | ["OK"] -> return ()
            | _ -> failwithf "Lua command failed: %A" response }

        member x.OscillateZ start max min stepsize time trigger =
            x.SendCommandAsync <| (sprintf "wobbleZ(%f,%f,%f,%f,%f,%A)" start max min stepsize time trigger)

        member x.PathZ points time trigger =
            let points' = "{ " + (List.reduce (sprintf "%s, %s") points) + " }"
            x.SendCommandAsync <| sprintf "pathZ( %s, %f, %A )" points' time trigger

        member x.SetMode (axis:Axis) (mode:PositionerMode) =
            x.SendCommandSync <| sprintf "%s.mode = %A" (luaAxis axis) mode

        interface IDisposable with
            member __.Dispose() = (luaPort :> IDisposable).Dispose()

    /// Directly issue commands to the ANC300
    type ControlPort(hostname) =
        let logger = log4net.LogManager.GetLogger("ANC300 Control")
        let handleOutput =
            sprintf "Returned: %s" >> logger.Debug

        let port = new TcpipInstrument("ANC300 control port",handleOutput,hostname,1028)

        let extractQueryResponse (lines : string list) =
            match List.tryLast lines with
            | Some "OK" ->
                List.truncate (lines.Length-1) lines
            | _ ->
                failwithf "ANC300 query failed"

        let extractMode lines =
            let r = new Regex(@"^mode\s*=\s*(\s+)")
            match extractQueryResponse lines with
            | [response] ->
                let m = r.Match(response)
                if m.Success then
                    m.Groups.[1].Value |> parseMode |> Some
                else
                    None
            | _ -> None

        let extractFrequency lines =
            let r = new Regex(@"^frequency\s*=\s*(\d+)")
            match extractQueryResponse lines with
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
            let r = new Regex(@"^voltage\s*=\s*(\d+\.)")
            match extractQueryResponse lines with
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
            "123456" |> port.WriteLine
            port.Start()

        member __.SendCommandSync command =
            let response = command |> port.QueryUntil (fun x -> x.StartsWith "> ")
            match response with
            | ["OK"] -> ()
            | _ -> failwithf "ANC300 command failed: %A" response

        member __.SendCommandAsync command = async {
            let! response = command |> port.QueryUntilAsync (fun x -> x.StartsWith "> ")
            match response with
            | ["OK"] -> return ()
            | _ -> failwithf "ANC300 command failed: %A" response }

        member __.Query query =
            let response = query |> port.QueryUntil (fun x -> x.StartsWith "> ")
            match List.tryLast response with
            | Some "OK" ->
                List.truncate (response.Length-1) response
            | _ ->
                failwithf "ANC300 query '%s' failed: %A" query response
        
        member x.GetMode (axis:Axis) =
            let response = sprintf "getm %d" (int axis) |> x.Query
            match extractMode response with
            | Some mode -> mode
            | None -> failwithf "Failed to get mode"

        member x.SetMode (axis:Axis) (mode:PositionerMode) =
            // Doesn't work for OSV!
            sprintf "setm %d %A" (int axis) mode |> x.SendCommandSync

        member x.SetOffsetVoltage (axis:Axis) (osv:float<V>) =
            sprintf "seta %d %f" (int axis) osv |> x.SendCommandSync

        member x.GetOffsetVoltage (axis:Axis) =
            let response = sprintf "geta %d" (int axis) |> x.Query
            match extractVoltage response with
            | Some voltage -> voltage
            | None -> failwithf "Failed to get offset voltage"

        member x.SetStepVoltage (axis:Axis) (v:float<V>) =
            sprintf "setv %d %f" (int axis) v |> x.SendCommandSync

        member x.GetStepVoltage (axis:Axis) =
            let response = sprintf "getv %d" (int axis) |> x.Query
            match extractVoltage response with
            | Some voltage -> voltage
            | None -> failwithf "Failed to get step voltage"

        member x.SetStepFrequency (axis:Axis) (f:int<Hz>) =
            sprintf "setf %d %d" (int axis) f |> x.SendCommandSync

        member x.GetStepFrequency (axis:Axis) =
            let response = sprintf "getf %d" (int axis) |> x.Query
            match response |> extractFrequency with
            | Some f -> f
            | None -> failwithf "Failed to get frequency"

        // Can fail if in the wrong mode. Runs in the background on device
        member x.Step (axis:Axis) direction count =
            let command = match direction with Up -> "stepu" | Down -> "stepd"
            sprintf "%s %d %d" command (int axis) count |> x.SendCommandAsync

        member x.Stop (axis:Axis) =
            axis |> int |> sprintf "stop %d" |> x.SendCommandSync

        member x.Wait (axis:Axis) =
            axis |> int |> sprintf "wait %d" |> x.SendCommandAsync

        interface IDisposable with
            member __.Dispose() = (port :> IDisposable).Dispose()

    /// Wraps up access to positioners in one interface
    /// Adds checks and compound commands
    type Positioner(control:ControlPort,lua:LuaPort,axis) =

        member __.Mode with get() = control.GetMode axis
                        and set mode = lua.SetMode axis mode

        abstract member VoltageLimit : unit -> float<V>
        default __.VoltageLimit() = 60.0<V>

        member x.MidRange() = x.VoltageLimit() * 0.5

        abstract member FrequencyLimit : unit -> int<Hz>
        default __.FrequencyLimit() = 2000<Hz>

        member x.Offset with get() = control.GetOffsetVoltage axis
                         and set v = if v > 0.0<V> && v < x.VoltageLimit() then
                                         control.SetOffsetVoltage axis v
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
                         | Step -> Parked
                         | OffsetVoltage -> Floating x.Offset
            and  set state = match state with
                             | Parked -> x.Mode <- Step
                             | Floating osv ->
                                x.Mode <- OffsetVoltage
                                x.Offset <- osv

        member x.Step direction count = async {
            let step = control.Step axis direction count
            match x.State with
            | Parked -> do! step
            | Floating osv ->
                x.State <- Parked
                do! step
                x.State <- Floating osv }

        /// Single triangular oscillation of the positioner
        member x.Oscillate depth points time trigger = async {
            let oscillateZ (centre:float<V>) amplitude points dwell trigger =
                let start = centre
                let voltageAmplitude = amplitude * x.VoltageLimit()
                let max   = if (centre + voltageAmplitude) <= x.VoltageLimit() then centre + voltageAmplitude else x.VoltageLimit()
                let min   = if (centre + voltageAmplitude) >= 0.0<V> then centre + voltageAmplitude else x.VoltageLimit()
                let stepsize = (max - min) / (float <| points-1)
                lua.OscillateZ start max min stepsize time trigger
            match x.State with
            | Parked ->
                x.State <- Floating <| x.MidRange()
                do! oscillateZ (x.MidRange()) depth points time trigger
                x.State <- Parked
            | Floating osv ->
                do! oscillateZ osv depth points time trigger }

        member __.Wait() = control.Wait axis
        member __.Stop() = control.Stop axis

        // Starts step operation then waits until it completes
        member x.StepAndWait direction count = async {
            do! x.Step direction count
            do! x.Wait() }

    type ANC300(hostname) =

        let controlPort = new ControlPort(hostname)
        let luaPort = new LuaPort(hostname)

        member __.X() = Positioner(controlPort,luaPort,Axis.X)
        member __.Y() = Positioner(controlPort,luaPort,Axis.Y)
        member __.Z() = Positioner(controlPort,luaPort,Axis.Z)

        member x.StopAll() =
            x.X().Stop()
            x.Y().Stop()
            x.Z().Stop()

        interface IDisposable with
            member __.Dispose() =
                (controlPort :> IDisposable).Dispose()
                (luaPort :> IDisposable).Dispose()
                
                
                
                
                
                
                
                
                
                
                
