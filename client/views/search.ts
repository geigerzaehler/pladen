import TemplateView = require('./base/template');

class SearchView extends TemplateView {
    get template()        { return 'search' }
    get elementTemplate() { return '<div class="search">' }

    // TODO distinguish between ui bindings and ui
    get uiBinding(): {[index: string]: string} { return {
        'search': 'input',
        'link':   'a',
        'downloadable': '.search-downloadable',
    }}

    get events(): {[index: string]: string} { return {
        'input  @ui.search': 'onChanged',
        'change @ui.search': 'onChanged',
        'click  @ui.downloadable': 'toggleDownloadable',
    }}

    ui: {
        link: JQuery;
        search: JQuery;
        downloadable: JQuery;
    }

    // TODO type bus
    constructor(public bus:any) {
        super();
        bus.client(this);
        // TODO should be this.app.router.search.enter
        this.bus.on('route:enter:search', 'set');
        this.delegateEvents();
        this.render();
    }

    set(val) {
        this.ui.search.val(val);
        this.onChanged();
    }

    onChanged() {
        var pattern:string = this.ui.search.val();
        if (pattern == '')
            pattern = null;

        var download:boolean = this.ui.downloadable.hasClass('checked')

        var filter = {
            pattern: pattern,
            download: download
        }

        if (pattern)
            // TODO use router for this
            this.ui.link.attr('href', '#/q/' + pattern);
        else
            this.ui.link.attr('href', null);

        this.app.search.dispatch(filter);
    }

    toggleDownloadable() {
        this.ui.downloadable.toggleClass('checked');
        this.onChanged();
    }

}

export = SearchView;
