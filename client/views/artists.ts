import underscore = require('underscore');
var each = underscore.each;
var some = underscore.some;
var map = underscore.map;

import css = require('css_promise');
import tpls = require('templates');


import Services = require('services');
import Provider = Services.Provider;
import DataTemplateView = require('./base/data_template');
import View = require('./base/view');
import filter = require('filter');
import Filter = filter.Filter;
import A = require('models/album');
import Album = A.Album;
import Track = require('models/track');
import Artist = require('models/artist');
import AlbumView = require('./album');
import AlbumExpansion = AlbumView.AlbumExpansion;

export = ArtistsView;

/**
 * This module contains the classes to build the list of artists and
 * their albums and tracks (releases).
 * 
 * `ArtistsView` is a collection view that manages the creation and
 * filtering of `ArtistView` instances.
 *
 * `ArtistView` is responsible for displaying all albums and single
 * tracks of an artist.
 *
 * `albumView` shows album information. It also has the
 * `AlbumExpansion` to display the tracks and provides interaction for
 * listening to those tracks.
 *
 * `trackView` just creates an element that shows the track title.
 */
// TODO merge this with AlbumCollectionView
class ArtistsView extends View {

    get elementTemplate() {
        return '<ol class=artist-list>';
    }

    constructor(public artists: Artist.Collection, private services: Provider) {
        super();
        this.app.search.add((f) => {
            this.filter(f);
        });
        artists.inserted.add((i) => {
            this.insert(i);
        });
    }

    render() {
        this.$el.empty();
        this.views = [];

        this.artists.each((artist) => {
            var view = this.createItemView(artist);
            this.views.push(view);
            this.$el.append(view.$el);
        })
        this.reFilter();
    }

    filter(f: filter.Filter) {
        this._filter = f;
        each(this.views, (a) => a.filter(f));
    }

    reFilter() {
        if (this._filter)
            this.filter(this._filter);
    }

    private insert(i: Artist.Index) {
        var view = this.createItemView(i.model);
        this.views.splice(i.index, 0, view);
        if (i.index == 0)
            this.$el.prepend(view.$el)
        else
            this.views[i.index-1].$el.after(view.$el)
    }

    private remove(index: number) {
        var removed = this.views.splice(index, 1);
        removed[0].destroy();
    }

    private createItemView(artist: Artist.Model) {
        return new ArtistView(artist, this.services);
    }

    private views: ArtistView[] = [];
    private _filter: Filter;
}


/**
 * Display the artist name and the list of albums and tracks
 */
class ArtistView extends DataTemplateView {
    get elementTemplate() {
        return '<li class=artist>';
    }

    get template() {
        return 'artist';
    }

    constructor(public artist: Artist.Model, private services: Provider) {
        super(artist);
        artist.changed.add(() => { this.render() });
        this.render();
    }

    render() {
        super.render();
        this._releases = [];

        var albumList = this.$('.artist-album-list').empty();
        each(this.artist.albums, (album) => {
            var view = new AlbumView(album, this.services);
            view.$el.appendTo(albumList);
            this._releases.push(view)
        })
        var trackList = this.$('.artist-track-list').empty();
        each(this.artist.tracks, (track) => {
            var view = new TrackView(track, this.services);
            view.$el.appendTo(trackList);
            this._releases.push(view)
        });
    }

    filter(f: Filter) {
        var matches = map(this._releases, (r) => r.filter(f));
        this.$el.toggleClass('filtered', !(some(matches)));
    }

    _releases: ReleaseView[];
}


interface ReleaseView {
    $el: JQuery;
    filter(f: Filter): boolean;
}


class AlbumView implements ReleaseView {
    constructor(a: Album, s: Provider) {
        this.album = a;
        this.$el = $(tpls.artistAlbum(a));

        var expansion = new AlbumExpansion(a, s);
        expansion.$el.appendTo(this.$el);

        var loading = this.$el.find('.album-loading');
        expansion.loading.add((loaded) => {
            css.transitionShow(loading, 'active');
            loaded.then(() => {
                css.transitionHide(loading, '-active');
            })
        })

        this.$el.on('click', 'button[data-action=toggle-album]',
                    () => expansion.toggle());
    }

    $el: JQuery;

    filter(f: Filter): boolean {
        var match = filter.album(f)(this.album);
        this.$el.toggleClass('filtered', !match)
        return match;
    }

    private album: Album
}


/**
 * Return an element that displays the track title
 */
class TrackView implements ReleaseView {
    constructor(t: Track.Track, s: Provider) {
        this.track = t;
        this.$el = $(tpls.artistTrack(t));

        if (t.downloadable) {
            this.$el.attr('draggable', 'true');
            this.$el.attr('data-track-id', t.id);
            s.dragTrack(this.$el, () => t);
            s.trackContextMenu(this.$el, () => t);
        }
    }

    $el: JQuery;

    filter(f: Filter): boolean {
        var match = filter.track(f)(this.track);
        this.$el.toggleClass('filtered', !match)
        return match;
    }

    private track: Track.Track;
}
