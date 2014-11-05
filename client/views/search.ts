/// <reference path="../../typings/bacon.d.ts" />

import Bacon = require('bacon');
import View = require('views/base/view2');
import dm = require('dom/mutators');
import dom = require('dom');
import tpls = require('templates');
import filter = require('filter');
import Filter = filter.Filter;

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

    // Search input
    var searchElement = view.$el.find('input[type=search]');
    var searchInputStream = dom.eventStream(searchElement, 'input')
    var searchChangeStream = dom.eventStream(searchElement, 'change');

    var search =
        searchInputStream.merge(searchChangeStream)
        .map(_ => searchElement.val())
        .toProperty('');

    // Downloadable checkbox
    var downloadableToggle = view.$el.find('.search-downloadable');
    var downloadable =
        dom.eventStream(downloadableToggle, 'click')
        .scan(false, (state, _) => !state);
    downloadable.assign(downloadableToggle, 'toggleClass', 'checked');


    view.filter = Bacon.combineTemplate<Filter>({
        downloadable: downloadable,
        search: search
    }).map(f => {
        if (f.downloadable != true)
            delete f.downloadable
        return f;
    });


    // Link to search URL
    var setSearchFragment = dm.attr('href', view.$el, 'a');
    view.searchFragment = new Bacon.Bus<string>();
    view.searchFragment.onValue((s) => {
        if (s)
            setSearchFragment('#/q/' + s);
        else
            setSearchFragment(null);
    });
    view.searchFragment.push('');
    view.searchFragment.plug(search);
    return view;
}
