import s = require('services');
import service = s.service;
import Dialogs = s.Dialogs;

import signals = require('dom/signals');
import Signal = signals.Signal;

import css = require('css_promise');
import View = require('views/base/view2');
import templates = require('templates');


export = dialogs;
var dialogs = service<Dialogs>('dialogs', () => new DialogManager());

class DialogManager extends View.View {

    constructor() {
        super($('<div class="dialogs modal inactive">'));
        this.$el.appendTo('body');
    }

    message(msg: string) {
        var $el = $(templates.messageDialog(msg));
        var close = signals.click($el, '[data-action=close]');
        this.open({
            $el: $el,
            close: close
        });
    }

    private open(d: Dialog) {
        var container = $('<dialog>')
        container.append(d.$el).appendTo(this.$el);

        container.addClass('run-popout');
        css.transitionShow(this.$el, '-inactive');

        d.close.add(() => {
            container.detach();
            this.close();
        });
    }

    private close() {
        css.transitionHide(this.$el, 'inactive');
    }
}

interface Dialog {
    close: Signal<void>;
    $el: JQuery;
}
