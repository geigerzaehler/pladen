/// <reference path="../../typings/bacon.d.ts" />
/// <reference path="../../typings/templates.d.ts" />

import Bacon = require('bacon');
import View = require('views/base/view2');
import tpls = require('templates');

/**
 * UI for search input and download toggle
 */
export class SearchView extends View.View {
    /**
     * Value of the search input
     */
    search: Bacon.Property<string>;

    /**
     * Value of the 'download' toggle
     */
    downloadable: Bacon.Property<boolean>;
    searchFragment: Bacon.Bus<string>;

    // FIXME backwards compat
    render() {}
    show() {}
    hide() {}
}

export function searchView() {
    var view = new SearchView($(tpls.search()));

    var searchInput = view.eventStream('input', 'input');
    var searchChange = view.eventStream('change', 'input');

    view.search =
        searchInput.merge(searchChange)
        .map((_) => view.$el.find('input').val())
        .toProperty('');

    view.downloadable =
        view.eventStream('click', '.search-downloadable')
        .scan(false, (state, _) => !state);
    view.downloadable.assign(view.$el.find('.search-downloadable'),
                             'toggleClass', 'checked');

    view.searchFragment = new Bacon.Bus<string>();
    view.searchFragment.onValue((s) => {
        if (s)
            view.$el.find('a').attr('href', '#/q/' + s);
        else
            view.$el.find('a').attr('href', null);
    });
    view.searchFragment.push('');
    view.searchFragment.plug(view.search);
    return view;
}
