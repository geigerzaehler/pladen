/// <reference path="../../typings/bacon.d.ts" />

import Bacon = require('bacon');
import View = require('views/base/view2');
import tpls = require('templates');
import filter = require('filter')
import Filter = filter.Filter

/**
 * UI for search input and download toggle
 *
 * Also exposes a perma-link to the current search.
 */
export class SearchView extends View.View {

    filter: Bacon.Property<Filter>;

    /**
     * Sets the search string for the search's perma-link.
     */
    searchFragment: Bacon.Bus<string>;


    // FIXME backwards compat
    render() {}
    show() {}
    hide() {}
}

export function searchView() {
    var view = new SearchView($(tpls.search));

    var searchInput = view.eventStream('input', 'input');
    var searchChange = view.eventStream('change', 'input');

    var search =
        searchInput.merge(searchChange)
        .map((_) => view.ui('input').val())
        .toProperty('');

    var downloadable =
        view.eventStream('click', '.search-downloadable')
        .scan(false, (state, _) => !state);
    downloadable.assign(view.ui('.search-downloadable'),
                        'toggleClass', 'checked');

    view.filter = Bacon.combineTemplate<Filter>({
        downloadable: downloadable,
        search: search
    }).map(f => {
        if (f.downloadable != true)
            delete f.downloadable
        return f;
    });

    view.searchFragment = new Bacon.Bus<string>();
    view.searchFragment.onValue((s) => {
        if (s)
            view.$el.find('a').attr('href', '#/q/' + s);
        else
            view.$el.find('a').attr('href', null);
    });
    view.searchFragment.push('');
    view.searchFragment.plug(search);
    return view;
}
