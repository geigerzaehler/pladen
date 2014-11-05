/// <reference path="../../typings/when/when.d.ts" />
import $ = require('jquery');
import w = require('when');
import css = require('css_promise');

/**
 * Overlay that can be removed when the application is finished
 * loading.
 */

export = splashScreen

function splashScreen(p: w.Promise<any>) {
    var $el = $('.app-loader');
    return p.delay(25).then(() => {
        return css.transitionHide($el, 'inactive')
    }).then(() => {
        $el.remove();
    }).catch( e => {
        $el.find('.app-loader-message')
        .html('Fehler beim Starten der Anwendung');
        throw e;
    });
}
