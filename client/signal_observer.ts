/// <reference path="../typings/signals.d.ts" />

export = SignalObserver;

class SignalObserver {

    constructor(private target?: any) {
    }

    on<T>(signal: Signal<T>, callback: (param:T)=>any) {
        var listener = callback.bind(this.target);
        this.listeners.push(listener);
        signal.add(listener);
    }

    // TODO once, start, stop

    private listeners: Array<()=>any> = [];
}
