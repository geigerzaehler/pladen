/// <reference path="../typings/bacon.d.ts" />
/// <reference path="../typings/jquery/jquery.d.ts" />
/// <reference path="../typings/underscore/underscore.d.ts" />
import Bacon = require('bacon')
import Stream = Bacon.Stream;
import _ = require('underscore');
import $ = require('jquery');
var find = _.find;
var each = _.each;


/**
 * This module includes various utitilies the simplify interaction with
 * the DOM.
 */


export function
eventStream($el: JQuery, event: string, selector?: string): Stream<JQueryEventObject> {
    // TODO add the function to bacon.d.ts
    var B: any = Bacon;
    return B.$.asEventStream.call($el, event, selector);
}


/**
 * Return a pair of observables that describe the state of a
 * drag-and-drop interaction on the element
 *
 * The function registers drag-and-drop event listeners on the element.
 * It is only interested in DragEvents with `type` in the list of data
 * transfer types. All other events are ignored
 *
 * If a 'dragenter' event with the correct type occurs on `el`, the `over`
 * property is set to `true`. Conversely, if a `dragleave` event occurs
 * it is set to `false`.
 *
 * If the 'drop' event is emitted, the value corresponding to `type` is
 * retrieved from the event's DataTransfer object and send to the
 * `drop` stream. The function prevents the default event handler for
 * drag events with matching type and thus enables the drop.
 */
export function dragDropStream(el: HTMLElement, type: string): DragDropStream {
    function eventFilter(ev: DragEvent) {
        var types:any = ev.dataTransfer.types;
        if (typeof types.contains == 'function')
            return types.contains(type);
        else
            return types.indexOf(type) >= 0;
    }

    function filteredEventStream(event: string) {
        return Bacon.fromEventTarget(el, event).filter(eventFilter);
    }

    var dragenter = filteredEventStream('dragenter');
    var dragleave = filteredEventStream('dragleave');
    var dragover = filteredEventStream('dragover')
                   .doAction((ev: DragEvent) => ev.preventDefault());
    var drop = filteredEventStream('drop')
               .map((ev: DragEvent) => ev.dataTransfer.getData(type));

    var over = Bacon.update(
        false
      , [dragenter], true
      , [dragleave], false
      , [drop], false
    );

    var draggedOver = {};

    // We join drop and dragover so that 'preventDefault' gets called
    // when we want to drop something.
    drop = Bacon.when<any>(
        [dragover], () => draggedOver
      , [drop], id
    ).filter( x => x !== draggedOver );

    return {drop: drop, over: over};
}

export interface DragDropStream {
    over: Bacon.Property<boolean>;
    drop: Bacon.Stream<string>;
}


export function voidSignal($el: JQuery, event: string, selector?: string) {
    return new Signal<void>($el, event, () => {}, selector);
}


/**
 * Create a signal that is dispatched when event is fired on the JQuery
 * element.
 *
 * Event handlers are only registered when the signal has listeners.
 */
export class Signal<T> {
    constructor($el: JQuery, event: string,
                selector?: string, transformer?: (e: JQueryEventObject) => T)
    constructor($el: JQuery, event: string,
                transformer?: (e: JQueryEventObject) => T, selector?: string)
    constructor($el: JQuery, event: string, selector: any, transformer: any) {
        if (typeof selector == 'string')
            this.selector = selector;
        else if (typeof transformer == 'string')
            this.selector = transformer;
        if (typeof selector == 'function')
            this.transform = selector;
        else if (typeof transformer == 'function')
            this.transform = transformer;

        if (typeof this.transform == 'undefined')
            this.transform == id;

        this.$el = $el;
        this.event = event;
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
        each(this.bindings.slice(), b => {
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

function id(x) { return x; }
