/// <reference path="../../../typings/tsd.d.ts" />
import M = require('backbone.marionette');
import $ = require('jquery');
import B = require('backbone');
import underscore = require('underscore');
var each = underscore.each;

import Global = require('global');

/**
 * A view is the programtic representation of a document element.
 *
 * The element is fixed, once and for all, in `$el` when the view is
 * instantiated. The view's main responsibility is to change the
 * elements content based on data.
 *
 * TODO clarify the next paragraphs
 *
 * This basic class introduces one key abstraction: A view is either
 * 'open' or 'closed'. In the 'open' state a view is visible to the
 * user and must be responsive to data updates.
 *
 * @event open    Element is inserted into DOM
 * @event close   Element is removed from DOM
 * @event update  Element content was changed
 *
 * TODO remove event aggregator
 */
class View extends B.Wreqr.EventAggregator {

    get elementTemplate(): string
    { return '<div>' }

    get events(): {[index: string]: string}
    { return {} }

    get uiBinding(): {[index: string]: string}
    { return {} }

    $el: JQuery;
    el:  HTMLElement;
    ui: any;
    app: Global;

    constructor() {
        super();
        this.$el = $(this.elementTemplate);
        this.el = this.$el[0];
        this.delegateEvents();
    }

    render() {
        return;
    }

    $(selector: string): JQuery {
      return this.$el.find(selector);
    }

    show(): void {}

    hide(): void {}

    /**
     * Remove element from the DOM and remove all references to
     * the element.
     *
     * This makes the view unusable and frees up memory
     */
    destroy(): void {
        this.$el.remove();
        this.$el = undefined;
        this.el = undefined;
    }


    delegateEvents() {
        var events = M.normalizeUIKeys(this.events, this.uiBinding);
        B.View.prototype.delegateEvents.call(this, events);
    }

    undelegateEvents() {
        B.View.prototype.undelegateEvents.call(this);
    }

    bindUIElements() {
        each(this.uiBinding, (selector, name) => {
            this.ui[name] = this.$(selector)
        })
    }

    unbindUIElements() {
        this.ui = {};
    }
}

export = View;
