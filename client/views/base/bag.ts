import View = require('./view');

/**
 * Display multiple views in one container.
 */
class BagView extends View {

    constructor(...views: View[]) {
        super();
        this.views = views;
        // FIXME calling this disables events in search view
        // this.render()
    }

    render() {
        this.views.forEach(view => view.render());
        var els = this.views.map(view => view.el);
        this.$el.empty().append(els);
    }

    show() {
        this.views.forEach(view => view.show());
    }

    hide() {
        this.views.forEach(view => view.hide());
    }

    private views: View[];
}

export = BagView;
