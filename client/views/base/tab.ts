import css = require('css_promise');
import when = require('when');
import resolve = when.resolve;
import Promise = when.Promise;

class TabView {
    selected: string;
    $el: JQuery;
    el: HTMLElement;

    constructor(tabs: {[index: string]: HTMLElement}) {
        this.$el = $('<ol>');
        this.el = this.$el[0];
        this.tabEls = {};

        for (var name in tabs) {
            var $tab = $('<li>')
                .attr('data-tab', name)
                .append(tabs[name])
                .addClass('inactive')
                .appendTo(this.$el);
            this.tabEls[name] = $tab;
        }
    }

    select(name:string): Promise<void> {
        var currentEl = this.tabEls[this.selected];
        var nextEl = this.tabEls[name];

        if (name == this.selected)
            return resolve<void>();

        return deactivate(currentEl).then(() => {
            this.selected = name;
            if (currentEl)
                return activate(nextEl);
            else
                return activateNow(nextEl);
        });
    }
    private tabEls: {[name: string]: JQuery}
}
export = TabView;


function activate(el?:JQuery): Promise<void> {
    if (!el)
        return resolve<void>();
    return css.transitionShow(el, '-inactive').yield(null);
}

function activateNow(el?:JQuery): Promise<void> {
    if (el)
        el.removeClass('inactive');
    return resolve<void>();
}

function deactivate(el?:JQuery): Promise<void> {
    if (!el)
        return resolve<void>();
    return css.transitionHide(el, 'inactive').yield(null);
}
