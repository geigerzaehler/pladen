import underscore = require('underscore');
var each = underscore.each;
var some = underscore.some;

import css = require('css_promise');
import DataTemplateView = require('./base/data_template');
import View = require('./base/view');
import query = require('query');
import A = require('models/album');
import Album = A.Album;
import Artist = require('models/artist');
import AlbumView = require('./album');
import AlbumExpansion = AlbumView.AlbumExpansion;

export = ArtistsView;

// TODO merge this with AlbumCollectionView
class ArtistsView extends View {

    get elementTemplate() {
        return '<ol class=artist-list>';
    }

    constructor(public artists: Artist.Collection) {
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

    filter(f: {pattern: string; download: boolean}) {
        this._filter = f;
        var pattern = f.pattern;
        var download = f.download;
        var matchAlbum = query.album(pattern);
        var matchTrack = query.track(pattern);

        each(this.views, (c) => {
            var albums = c.model.albums;
            var matchedAlbums = c.model.albums.some((a) => {
                return matchAlbum(a) && (!download || a.downloadable)
            });
            var matchedTracks = !download && some(c.model.tracks, matchTrack)
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
        return new ArtistView(artist);
    }

    private views: ArtistView[] = [];
    private _filter: {pattern: string; download: boolean};
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

    constructor(public artist: Artist.Model) {
        super(artist);
        artist.changed.add(() => { this.render() });
        this.render();
    }

    render() {
        super.render();
        var albumList = this.$('.artist-album-list');
        each(this.artist.albums, (album) => {
            var view = new AlbumView(album);
            view.render();
            albumList.append(view.el);
        })
        var trackList = this.$('.artist-track-list');
        each(this.artist.tracks, (track) => {
            var view = new TrackView(track);
            view.render();
            trackList.append(view.el);
        });

    }
}

class AlbumView extends DataTemplateView {
    get elementTemplate() {
        return '<li class=artist-album>';
    }

    get template() {
        return 'artist-album';
    }

    get events(): {[ev: string]: string} { return {
        'click button': 'toggleExpansion'
    }}

    constructor(public album: Album) {
        super(album);
        this.expansion = new AlbumExpansion(album);
        this.expansion.loading.add((loaded) => {
            css.transitionShow(this.loading, 'active');
            loaded.then(() => {
                css.transitionHide(this.loading, '-active');
            })
        })
    }

    toggleExpansion() {
        this.expansion.toggle();
    }

    render() {
        super.render();
        this.$el.append(this.expansion.$el);
        this.loading = this.$('.album-loading');
    }

    private expansion: AlbumExpansion;
    private loading: JQuery;
}

class TrackView extends DataTemplateView {
    get elementTemplate() {
        return '<li class=artist-track>';
    }

    get template() {
        return 'artist-track';
    }
}
