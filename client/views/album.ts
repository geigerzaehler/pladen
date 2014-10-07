import moment = require('moment');
import w = require('when');
import _ = require('underscore');
import Promise = w.Promise;
import cssp = require('css_promise');
import Signal = require('signals');
import SignalObserver = require('signal_observer');

import A = require('models/album');
import ModalManager = require('./modal_manager');
import View = require('./base/view');
import DataTemplate = require('./base/data_template');
import Track = require('models/track');

import c = require('utils/collection');

import R = require('models/release');
import B = require('models/base');


export class ReleaseCollection extends View {
    get elementTemplate()
    { return '<ol class="releases">' }

    constructor(private collection: B.List<R.Release>) {
        super();
        this.render();
        this.collectionObserver.on(collection.inserted, this.insert);
    }

    render() {
        this.$el.empty();
        this.views = [];
        _.each(this.collection.items, (item) => {
            var view = this.createItemView(item);
            if (!view)
                return;

            this.views.push(view);
            this.$el.append(view.$el);
        })
    }

    private insert(is: R.Index[]) {
        var views = c.mapIndices(is, (r) => {
            return this.createItemView(r);
        });
        var els = c.mapIndices(views, (view) => {
            return view.$el;
        });
        c.insertIndices(this.views, views);
        c.insertDomIndices(this.$el, els);
    }

    private createItemView(r: R.Release): DataTemplate {
        if (r.album)
            return new AlbumView(r.album);
        else
            return new TrackView(r.track)
    }

    private views: DataTemplate[] = [];
    private collectionObserver = new SignalObserver(this);
}


export class TrackView extends DataTemplate {
    get elementTemplate()
    { return '<li class=track>' }

    get template()
    { return 'track-release' }

    constructor(private track: Track.Track) {
        super(track);
        this.render();

        this.$el.on('dragstart', '.release-head', (e:any) => {
            var dt = e.originalEvent.dataTransfer;
            dt.dropEffect = 'none';
            dt.effectAllowed = 'copy';
            dt.setData('application/x-play-track',
                       JSON.stringify(Track.pickAttributes(this.track)));
            var rd = ReleaseDrag.instance;
            rd.track = track;
            dt.setDragImage(rd.el, 0, 0);

            $('html').addClass('drag-track');
        });
        this.$el.on('dragend', '.release-head', (e:any) => {
            $('html').removeClass('drag-track');
        });
    }

    render() {
        super.render();
        if (this.track.downloadable)
            this.$('.release-head').attr('draggable', 'true');
    }

    helper(model: any) {
        var added = model.added;
        return {
            added:     added.fromNow(),
            addedIso:  added.format('LLL')
        };
    }
}


export class ReleaseDrag extends DataTemplate {

    get elementTemplate()
    { return '<div class=drag-release>' }

    get template()
    { return 'drag-release' }

    static get instance() {
        if (!this._instance) {
            this._instance = new ReleaseDrag(null);
            this._instance.$el.appendTo('.hidden-box');
        }
        return this._instance;
    }

    set track(t: Track.Track) {
        this.release = {
            name: t.title,
            artist: t.artist
        }
    }


    private set release(m: {name: string; artist: string}) {
        this.model = m;
        this.render();
    }

    private static _instance: ReleaseDrag;
}


/**
 * Show album information.
 *
 * Show the album’s name, the artist, and the number of days
 * since this album was added.
 *
 * Clicking on the view toggles the expansion containing the
 * track list and the download button.
 */
export class AlbumView extends DataTemplate {
    get template()
    { return 'album' }

    get elementTemplate()
    { return '<li class=album>' }

    get events(): {[index: string]: string} { return {
        'click .release-head': 'toggleExpansion'
    }}

    constructor(public model: A.Album) {
        super(model);
        this.expansion = new AlbumExpansion(model);
        this.expansion.loading.add((loaded) => {
            cssp.transitionShow(this.loading, 'active');
            loaded.then(() => {
                cssp.transitionHide(this.loading, '-active');
            })
        })
        this.render();
    }

    render() {
        super.render();
        this.$el.append(this.expansion.$el);
        this.loading = this.$('.album-loading');
        this.toggleExpansion(this.showExpansion);
    }

    helper(model: any) {
        var added = model.added;
        return {
            added:     added.fromNow(),
            addedIso:  added.format('LLL')
        };
    }

    private toggleExpansion(show?: boolean) {
        this.expansion.toggle(show);
    }

    private showExpansion: boolean = false;
    private expansion: AlbumExpansion;
    private loading: JQuery;
}


/**
 * Track list and download button for an album
 *
 * This view is hidden by default. The 'show' method fetches
 * the track data for the album and then slides the element open.
 * The method returns a promise that is resolved when the data
 * has been loaded.  To hide the track list with an animation
 * call 'hide'.
 *
 * Clicking on the download button calls the album’s download
 * method.
 */
export class AlbumExpansion extends DataTemplate {
    get template()
    { return 'album-tracks' }

    get elementTemplate()
    { return '<div class=album-tracks>' }

    get events(): {[index: string]: string} { return {
        'click .album-download': 'download'
    }}

    /**
     * Dispatched when the album tracks are loaded.
     *
     * The promise is resolved, when the loading is finished;
     */
    loading = new Signal<Promise<void>>();

    constructor(public album: A.Album) {
        super(album);
        album.changed.add(() => {
            this.render();
        })

        this.$el.on('dragstart', '.album-track', (e:any) => {
            var dt = e.originalEvent.dataTransfer;
            dt.dropEffect = 'none';
            dt.effectAllowed = 'copy';

            var trackId = e.target.attributes['data-id'].value;
            var track = _.find(album.tracks, (track) => {
                return track.id == trackId;
            })
            var rd = ReleaseDrag.instance;
            rd.track = track;
            dt.setDragImage(rd.el, 0, 0);
            dt.setData('application/x-play-track',
                       JSON.stringify(Track.pickAttributes(track)));

            $('html').addClass('drag-track');
        });
        this.$el.on('dragend', '.album-track', (e:any) => {
            $('html').removeClass('drag-track');
        });
    }

    toggle(show?: boolean) {
        if (typeof show != 'boolean')
            show = !this.shown;
        if (show)
            this.show();
        else
            this.hide();
    }

    render() {
        super.render();
        if (this.album.downloadable)
            this.$('.album-track').attr('draggable', 'true');
    }

    show() {
        this.shown = true;
        this.ensureTracks().then(() => {
            cssp.transitionToAutoHeight(this.$el);
        });
    }

    hide() {
        this.shown = false;
        cssp.transitionFromAutoHeight(this.$el, 0)
    }

    download(): boolean {
        this.album.download().catch(() => {
            this.app.openNoAlbumDownload(this.album);
        }).done();
        return false;
    }

    helper(data: any) {
        var albumArtist = data.artist
        var tracks = data.tracks
        if (!tracks)
            return;

        tracks = _.map(tracks, (track: Track.Attributes) => {
            track = Track.pickAttributes(track);
            if (track.artist.indexOf(albumArtist) > -1)
                track.artist = track.artist.substring(albumArtist.length);
            else if (albumArtist.indexOf(track.artist) > -1)
                track.artist = "";
            return track;
        });
        return {tracks: tracks};
    }

    private ensureTracks(): Promise<void> {
        if (!this.album.tracks) {
            var loaded = this.album.fetch();
            this.loading.dispatch(loaded);
            return loaded;
        }
        else
            return w.resolve<void>();
    }

    private shown = false;
}
