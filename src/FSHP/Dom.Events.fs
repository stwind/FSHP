namespace FSHP.Dom.Events

open Microsoft.FSharp.Control
open FSHP.Helper
open System
open System.Collections.Generic

type EventPhaseType =
    | CapturingPhase = 1
    | AtTarget = 2
    | BubblingPhase = 3

type IEvent =
    abstract Type : string
    abstract Target : IEventTarget
    abstract CurrentTarget : IEventTarget
    abstract EventPhase : int
    abstract StopPropagation : unit -> unit
    abstract StopImmediatePropagation : unit -> unit
    abstract Bubbles : bool
    abstract Cancelable : bool
    abstract PreventDefault : unit -> unit
    abstract DefaultPrevented : bool
    abstract IsTrusted : bool

and IEventTarget =
    abstract AddEventListener : string * EventListener * bool -> unit
    abstract RemoveEventListener : string * EventListener * bool -> unit
    abstract DispatchEvent : IEvent -> bool

and DomEvent(eType: string, bubbles : bool, cancelable : bool) =
    inherit EventArgs()
    let eType = eType
    let mutable target = Unchecked.defaultof<IEventTarget>
    let mutable currTarget = Unchecked.defaultof<IEventTarget>
    let mutable eventPhase = 2
    let mutable stopPropFlag = false
    let mutable stopImmePropFlag = false
    let bubbleFlag = bubbles
    let cancelFlag = cancelable
    let mutable canceledFlag = false
    let mutable trustedFlag = false
    let mutable initializedFlag = false
    let mutable dispatchFlag = false
    member internal this.SetTarget value = target <- value
    member internal this.SetCurrTarget value = currTarget <- value
    member internal this.SetEventPhase value = eventPhase <- value
    interface IEvent with
        member this.Type = eType
        member this.Target = target
        member this.CurrentTarget = currTarget
        member this.EventPhase = eventPhase
        member this.StopPropagation() = stopPropFlag <- true
        member this.StopImmediatePropagation() = 
            (this :> IEvent).StopPropagation()
            stopImmePropFlag <- true
        member this.Bubbles = bubbleFlag
        member this.Cancelable = cancelFlag
        member this.PreventDefault() = if cancelFlag then canceledFlag <- true
        member this.DefaultPrevented = canceledFlag
        member this.IsTrusted = trustedFlag


and EventListener = delegate of obj * DomEvent -> unit

//and EventTarget() =
    //let mutable allListeners = new Dictionary<string, EventListener list>()
    //interface IEventTarget with
        //member this.AddEventListener(lType, listener, captrue:bool) =
            //let mutable listeners = List.empty<EventListener>
            //if not (allListeners.TryGetValue(lType, &listeners))
                //then allListeners.Add(lType, listeners)
            //allListeners.[lType] <- listener :: listeners
        //member this.RemoveEventListener(lType, listener, captrue:bool) =
            //let mutable listeners = List.empty<EventListener>
            //if allListeners.TryGetValue(lType, &listeners)
                //then
                    //allListeners.[lType] <- removeItem listener (===) listeners
        //member this.DispatchEvent (ev:IEvent) =
            //let mutable listeners = List.empty<EventListener>
            //if allListeners.TryGetValue(ev.Type, &listeners)
                //then 
