/// <reference path="../../typings/bacon.d.ts" />
import Bacon = require('bacon');
import View = require('views/base/view2');

export class SearchView extends View.View {
    search: Bacon.Property<string>;
    downloadable: Bacon.Property<boolean>;
    searchFragment: Bacon.Bus<string>;

    // FIXME backwards compat
    render() {}
    show() {}
    hide() {}
}

export function searchView() {
    var view = <SearchView>View.templateView('<div class="search">', 'search');

    var searchInput = view.eventStream('input', 'input');
    var searchChange = view.eventStream('change', 'input');

    view.search =
        searchInput.merge(searchChange)
        .map((_) => view.$el.find('input').val())
        .toProperty('');
    view.downloadable =
        View.eventStream(view, 'click', '.search-downloadable')
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
