module Mainloop
open Fable.Core

type IMainLoop =
    abstract getFPS: unit -> float
    abstract getMaxAllowedFPS: unit -> int32
    abstract getSimulationTimestep: unit -> int32
    abstract isRunning: unit -> bool
    abstract resetFrameDelta: unit -> float
    /// `abstract setBegin(begin: (timestamp: number, delta: number) => void): MainLoop`
    abstract setBegin: ``begin``:(float -> int32 -> unit) -> IMainLoop
    /// `abstract setDraw(draw: (interpolationPercentage: number) => void): MainLoop`
    abstract setDraw: draw:(float -> unit) -> IMainLoop
    /// `abstract setUpdate(update: (delta: number) => void): MainLoop`
    abstract setUpdate: update:(float -> unit) -> IMainLoop
    /// `abstract setEnd(end: (fps: number, panic: boolean) => void): MainLoop`
    abstract setEnd: ``end``: (float -> bool -> unit) -> IMainLoop
    abstract setMaxAllowedFPS: ?fps: float -> IMainLoop
    abstract setSimulationTimestep: timestep:int32 -> IMainLoop
    abstract start: unit -> IMainLoop
    abstract stop: unit -> IMainLoop

[<ImportAll("mainloop.js")>]
let mainloop : IMainLoop = jsNative
