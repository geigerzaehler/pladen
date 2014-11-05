/// <reference path="../../typings/jquery/jquery.d.ts" />

export interface Signal<T> {
    add(l: (x:T) => void): () => void;
    once(l: (x:T) => void): () => void;
}


export function click($el, selector?: string): Signal<void> {
    return create($el, 'click', selector);
}


export function
create<T>($el: JQuery, event: string, selector: string,
          transformer: (e: JQueryEventObject) => T): Signal<T>;
export function
create($el: JQuery, event: string, selector?: string): Signal<void>;
export function
create<T>($el: JQuery, event: string, selector?, transformer?): Signal<T> {
    if (typeof transformer == 'undefined')
        transformer = noop;
    return new CSignal<T>($el, event, selector, transformer)
}



class CSignal<T> implements Signal<T> {
    constructor($el: JQuery, event: string,
                selector: string, transformer: (e: JQueryEventObject) => T) {
        this.$el = $el;
        this.event = event;
        this.selector = selector;
        this.transform = transformer;
        this.bindings = [];
        this.dispatcher = this.dispatch.bind(this);
    }

    add(l: (param:T) => void): () => void {
        var binding = new SignalBinding<T>(l);
        this.bindings.push(binding)
        this.activate();
        return () => this.remove(binding);
    }

    once(l: (param:T) => void): () => void {
        var binding = new SignalBinding<T>(l, true);
        this.bindings.push(binding)
        this.activate();
        return () => this.remove(binding);
    }

    private dispatch(e: JQueryEventObject) {
        this.bindings.slice().forEach( b => {
            b.listener(this.transform(e));
            if (b.once)
                this.remove(b);
        });
    }

    private remove(b: SignalBinding<T>) {
        var i = this.bindings.indexOf(b)
        if (i >= 0)
            this.bindings.splice(i, 1);
        if (this.bindings.length == 0)
            this.deactivate();
    }

    private activate() {
        if (this.active)
            return;
        this.$el.on(this.event, this.selector, this.dispatcher)
        this.active = true;
    }

    private deactivate() {
        if (!this.active)
            return;
        this.$el.off(this.event, this.selector, this.dispatcher)
        this.active = false;
    }

    private $el: JQuery;
    private event: string;
    private selector: string;
    private active: boolean;
    private bindings: SignalBinding<T>[];
    private transform: (e: JQueryEventObject) => T;
    private dispatcher: (e: JQueryEventObject) => void;
}


class SignalBinding<T> {
    constructor(l: (x:T) => void, once = false) {
        this.listener = l;
        this.once = once;
    }

    listener: (x:T) => void;
    once: boolean;
}

function noop() {}
