import View = require('./view');
import css = require('css_promise');
import when = require('when');
import resolve = when.resolve;
import Promise = when.Promise;

interface TabViews {[index:string]:View}
interface TabEls {[index:string]:JQuery}

class TabView extends View {
    tabTagName = 'li';
    selected:string;

    constructor(tabViews: {[index: string]: View}) {
        super();
        this.tabViews = tabViews;
        this.render();
    }

    render() {
        this.$el.empty();
        for (var name in this.tabViews) {
            var view = this.tabViews[name];
            // TODO remove this
            view.render();
            var $el = $('<' + this.tabTagName + '>')
                .attr('data-tab', name)
                .append(view.$el)
                .addClass('inactive')
            this.$el.append($el);
            this.tabEls[name] = $el;
        }
    }

    select(name:string): Promise<View> {
        var currentEl = this.tabEls[this.selected];
        var currentView = this.tabViews[this.selected];
        var nextEl = this.tabEls[name];
        var nextView = this.tabViews[name];

        return deactivate(currentEl).then(() => {
            this.selected = name;

            if (currentView)
                currentView.hide();
            if (nextView)
                nextView.show();

            var activate_: (el:JQuery)=>Promise<void>;
            if (currentView)
                activate_ = activate;
            else
                activate_ = activateNow;
            return activate_(nextEl)
        }).yield(nextView);
    }

    private tabViews:TabViews = {};
    private tabEls:TabEls = {};
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
