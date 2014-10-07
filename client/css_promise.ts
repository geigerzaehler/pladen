/// <reference path="../typings/tsd.d.ts" />

/**
 * This module contains Promise interfaces for DOM transitions and
 * animations.
 *
 * It also includes functions that trigger special transitions.
 */
import underscore = require('underscore');
import when = require('when');
import Promise = when.Promise;
import promise = when.promise;

var map = underscore.map;
var max = underscore.max;

var animationendEvent =
      'webkitAnimationEnd'
    + ' animationend'
    + ' oanimationend'
    + ' MSAnimationEnd'

export function animationEnd(el: JQuery) {
    return promise<Event>((resolve, reject) => {
        el.one(animationendEvent, resolve)
    });
}


export function transitionEnd(el: JQuery, property = 'all', timeout = false) {
    var p = promise<Event>((resolve, reject) => {
        el.one('transitionend.promise', resolve)
    });
    if (timeout) {
        var durations = window
            .getComputedStyle(el[0])['transition-duration']
            .replace(/ms/g, '')
            .replace(/s/, '000')
            .split(/ *, */);
        var duration = max(map(durations, parseInt));
        p = p.timeout(duration)
    }
    return p;
}


/**
 * Compute the auto height of the element and transition to it.
 *
 * Resolves the returned promise when the transition has completed.
 */
export function transitionToAutoHeight($el: JQuery) {
    if (!$el.length) return;

    $el = $el.eq(0);
    var el = $el[0]
    var oldHeight = el.clientHeight

    $el.css('height', 'auto');
    var newHeight = el.clientHeight

    $el.css('height', oldHeight)
    el.clientHeight

    $el.css('height', newHeight)
    return transitionEnd($el).finally(() => {
        $el.css('height', 'auto');
    });
}


export function transitionFromAutoHeight($el: JQuery, to: number) {
    if (!$el.length) return;

    $el = $el.eq(0);
    var el = $el[0];

    var currentHeight = $el.css('height');
    $el.css('height', currentHeight);

    el.clientHeight
    $el.css('height', to);
    return transitionEnd($el).then(() => {
        $el.css('height', to);
    });
}


/**
 * Changes class of the element but keep the current display value
 * until the transition is finished.
 */
export function transitionHide($el: JQuery, klasses: string, timeout: number = 1000) {
    var display = $el.css('display');
    $el.css('display', display);
    var end = transitionEnd($el)
    end.timeout(timeout)
    .catch(() => {})
    .finally(() => {
        $el.css('display', '');
    });
    changeClass($el, klasses);
    return end;
}


/**
 * Force the element to be laid out and then change the class to
 * trigger transitions. Returns a promise to the transition end.
 *
 * This is useful if a class sets the 'display' style to 'block' and
 * changes properties that need to transition. This would normally not
 * render the transition.
 *
 * @param classes
 *   Space separated string of class names to add to the element.
 *   If a class name starts with '-' it is removed from the element.
 */
export function transitionShow($el: JQuery, classes: string) {
    $el.css('display', 'block');
    $el[0].clientHeight;
    $el.css('display', '');
    changeClass($el, classes);
    return transitionEnd($el);
}


function changeClass($el: JQuery, klasses: string) {
    var add: string[] = [];
    var remove: string[] = [];
    klasses.split(' ').forEach((kl) => {
        if (kl[0] == '-')
            remove.push(kl.slice(1));
        else
            add.push(kl);
    });
    return $el.removeClass(remove.join(' '))
              .addClass(add.join(' '));
}
