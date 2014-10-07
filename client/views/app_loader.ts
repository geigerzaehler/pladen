import css = require('css_promise');

/**
 * Overlay that can be removed when the application is finished
 * loading.
 */
class AppLoaderView {

    constructor(public $el:JQuery) {
        this.messageEl = $el.find('.app-loader-message');
    }

    finish(): void {
        css.transitionHide(this.$el, 'inactive')
        .then(() => {
            this.$el.detach();
            this.$el = null;
        }).done()
    }

    fail(): void {
        this.messageEl.html('Fehler beim Starten der Anwendung');
    }

    private messageEl: JQuery;

}

export = AppLoaderView
