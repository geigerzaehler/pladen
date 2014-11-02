import moment = require('moment');
import w = require('when');
import _ = require('underscore');
import Promise = w.Promise;
import cssp = require('css_promise');
import services = require('services');
import Provider = services.Provider;
import Player = services.Player;

import Signal = require('signals');
import SignalObserver = require('signal_observer');
import templates = require('templates');

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

    constructor(private collection: B.List<R.Release>, p: Provider) {
        super();
        this.provider = p;
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
            return new AlbumView(r.album, this.provider);
        else
            return new TrackView(r.track, this.provider)
    }

    private views: DataTemplate[] = [];
    private collectionObserver = new SignalObserver(this);
    private provider: Provider;
}


export class TrackView extends DataTemplate {
    get elementTemplate()
    { return '<li class=track>' }

    get template()
    { return 'track-release' }

    constructor(private track: Track.Track, services: Provider) {
        super(track);
        this.render();

        var dragTrack = services.get('drag-track');
        this.$el.on('dragstart', '.release-head', (e:any) => {
            dragTrack(this.track, e.originalEvent.dataTransfer);
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

    constructor(public model: A.Album, p: Provider) {
        super(model);
        this.expansion = new AlbumExpansion(model, p);
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

    constructor(public album: A.Album, p: Provider) {
        super(album);
        album.changed.add(() => {
            this.render();
        })

        if (this.album.downloadable)
            this.$el.on('click', '.album-track', function() {
                var track = trackFromId($(this).attr('data-id'));
                trackContextMenu(track, $(this), p.get('player'));
            })

        function trackFromId(id: any) {
            return _.find(album.tracks, (track) => {
                return track.id == id;
            });
        }

        // TODO remove code duplication
        var dragTrack = p.get('drag-track');
        this.$el.on('dragstart', '.album-track', (e:any) => {
            var dt = e.originalEvent.dataTransfer;
            var track = trackFromId(e.target.attributes['data-id'].value);
            dragTrack(track, dt)
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


function trackContextMenu(track: Track.Attributes, $parent: JQuery, player: Player) {
    var $el = $(templates.trackContextMenu());
    $el.on('click', ['data-action=play'], function() {
        player.play(track);
    })
    $parent.after($el);
    setTimeout(function() {
        $(document).one('click', function() {
            $el.remove();
        })
    }, 0);
}
