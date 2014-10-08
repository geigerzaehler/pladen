/// <reference path="../../../typings/jquery/jquery.d.ts" />
/// <reference path="../../../typings/bacon.d.ts" />
/// <reference path="../../../typings/mustache/mustache.d.ts" />
import Bacon = require('bacon');
import $ = require('jquery');
import elementEventStream = require('utils/element_event_stream');
import Mustache = require('mustache');


export class View {

    constructor($el) {
        this.$el = $el;
        this.el = $el[0];
        this.eventStreams = {};
        this.uiCache = {}
    }

    el: HTMLElement;
    $el: JQuery;
    eventStreams: {[selector:string]: {[event: string]: Bacon.Stream<Event>}};
    uiCache: {}

    $(selector: string) {
        return this.$el.find(selector);
    }

    ui(selector: string) {
        var $el = this.uiCache[selector];
        if (!$el)
            $el = this.uiCache[selector] = this.$(selector);

        return $el;
    }

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
}

export function eventStream(view: View, event: string, selector = "") {
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

export function mustacheRender
(template: string, data: Bacon.Observable<{}>): Bacon.Observable<string> {
    return data.map((d) => {
        return Mustache.render(template, d);
    })
}

export function setContent(view: View, content: Bacon.Observable<string>) {
    return content.onValue((c) => {
        view.el.innerHTML = c;
    })
}

export function templateView(elementTemplate: string, templateName: string): View {
    var template = $("template[name='" + templateName + "']").html();
    var $el = $(elementTemplate).html(template);
    return new View($el);
}
