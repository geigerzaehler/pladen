interface SignalBinding {
    detach():void;
}

declare class Signal<T> {
    add(listener: (param:T) => void): SignalBinding;
    addOnce(listener: (param:T) => void): SignalBinding;
    dispatch(): void;
    dispatch(param:T): void;
}

declare module "signals" {
    export = Signal;
}
