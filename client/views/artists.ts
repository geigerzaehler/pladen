import underscore = require('underscore');
var each = underscore.each;
var some = underscore.some;

import css = require('css_promise');
import tpls = require('templates');


import Services = require('services');
import DataTemplateView = require('./base/data_template');
import View = require('./base/view');
import query = require('query');
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

    constructor(public artists: Artist.Collection, private services: Services.Provider) {
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
        var matchAlbum = filter.album(f);
        var matchTrack = filter.track(f);

        each(this.views, (c) => {
            var matchedAlbums = c.model.albums.some(matchAlbum)
            var matchedTracks = c.model.tracks.some(matchTrack)
            c.$el.toggleClass('filtered', !(matchedAlbums || matchedTracks));
        });
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

    constructor(public artist: Artist.Model, private services: Services.Provider) {
        super(artist);
        artist.changed.add(() => { this.render() });
        this.render();
    }

    render() {
        super.render();
        var albumList = this.$('.artist-album-list');
        each(this.artist.albums, (album) => {
            albumView(album, this.services).appendTo(albumList);
        })
        var trackList = this.$('.artist-track-list');
        each(this.artist.tracks, (track) => {
            trackView(track, this.services).appendTo(trackList);
        });
    }
}


function albumView(a: Album, p: Services.Provider): JQuery {
    var $el = $(tpls.artistAlbum(a));

    var expansion = new AlbumExpansion(a, p);
    expansion.$el.appendTo($el);

    var loading = $el.find('.album-loading');
    expansion.loading.add((loaded) => {
        css.transitionShow(loading, 'active');
        loaded.then(() => {
            css.transitionHide(loading, '-active');
        })
    })

    $el.on('click', 'button[data-action=toggle-album]',
           () => expansion.toggle());

    return $el;
}


/**
 * Return an element that displays the track title
 *
 * TODO Make it possible to listen to a track.
 */
function trackView(t: Track.Track, s: Services.Provider): JQuery {
    var $el = $(tpls.artistTrack(t));

    if (t.downloadable) {
        $el.attr('draggable', 'true');

        // TODO Use static typing for provider
        var dragTrack = s.get('drag-track');
        $el.on('dragstart', (e:any) => {
            dragTrack(t, e.originalEvent.dataTransfer)
            $('html').addClass('drag-track');
        });
        $el.on('dragend', (e:any) => {
            $('html').removeClass('drag-track');
        });
    }
    return $el;
}
