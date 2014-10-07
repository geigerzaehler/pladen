/// <reference path="../typings/tsd.d.ts" />
import $ = require('jquery');

export = oneline;

/**
 * Create tooltips for truncated text.
 *
 * @css oneline, tooltip
 *
 * TODO better name
 */
function oneline(root: JQuery) {
    $(root).on('mouseenter.oneline', '.oneline', (ev) => {
        var $this = $(ev.target);

        makeTooltip($this);
        $this.one('mouseleave', () => {
            removeTooltip($this);
        });

    }).on('click.oneline', '.oneline', (ev) => {
        var $this = $(ev.target);
        removeTooltip($this) || makeTooltip($this);
    });
}

function makeTooltip(el: JQuery) {
    if (el.data('tooltip') || el.width() < el.parent().width())
        return;

    var tooltip = $('<div class=tooltip>');
    el.data('tooltip', tooltip);
    tooltip.html(el.html())
           .appendTo(el)
}

function removeTooltip(el: JQuery): boolean {
    var tooltip: JQuery = el.data('tooltip');
    if (!tooltip)
        return false;

    tooltip.remove();
    el.data('tooltip', null);
    return true;
}
