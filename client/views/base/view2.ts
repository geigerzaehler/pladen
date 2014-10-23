/// <reference path="../../../typings/jquery/jquery.d.ts" />
/// <reference path="../../../typings/bacon.d.ts" />

import Bacon = require('bacon');
import $ = require('jquery');
import elementEventStream = require('utils/element_event_stream');


/**
 * Encapsulate a DOM node and provide a basic interface to events and
 * sub-nodes.
 */
export class View {

    constructor($el) {
        this.$el = $el;
        this.el = $el[0];
        this.eventStreams = {};
        this.uiCache = {}
    }

    el: HTMLElement;
    $el: JQuery;

    $(selector: string) {
        return this.$el.find(selector);
    }


    /**
     * Cached version of `this.$()`.
     */
    ui(selector: string) {
        var $el = this.uiCache[selector];
        if (!$el)
            $el = this.uiCache[selector] = this.$(selector);

        return $el;
    }


    /**
     * Obtain an event stream for a DOM event on this node.
     *
     * Uses delegated events on the root node `this.el` and caches
     * event streams.
     */
    eventStream(event: string, selector = "") {
        var view = this;
        var selected = view.eventStreams[selector];
        if (selected == undefined) {
            selected = {};
            view.eventStreams[selector] = selected;
        }
        var stream = selected[event]
        if (stream == undefined) {
            stream = elementEventStream(view.el, event, selector);
            selected[event] = stream;
        }
        return stream;
    }

    private eventStreams: {[selector:string]: {[event: string]: Bacon.Stream<Event>}};
    private uiCache: {};
}
