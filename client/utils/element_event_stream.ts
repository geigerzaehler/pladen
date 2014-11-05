/// <reference path="../../typings/jquery/jquery.d.ts" />
/// <reference path="../../typings/bacon.d.ts" />
import Bacon = require('bacon');
import $ = require('jquery');

export = elementEventStream;

/**
 * TODO this is made obsolete by the 'dom' module.
 */

function elementEventStream
(el: HTMLElement, event: string, selector?: string): Bacon.Stream<Event> {
    var B: any = Bacon;
    return B.$.asEventStream.call($(el), event, selector);
}
