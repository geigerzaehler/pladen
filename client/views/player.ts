/// <reference path="../../typings/bacon.d.ts" />
import Bacon = require('bacon')
import moment = require('moment');

import services = require('services');
import service = services.service;
import Service = services.Service;
import Backend = require('../player');

import css = require('css_promise');
import dom = require('dom');
import dm = require('dom/mutators');
import splice = require('utils/splice');
import templates = require('templates');

import ViewModule = require('views/base/view2');
import View = ViewModule.View;

import Track = require('models/track');


class Player extends View {
    constructor(audio: Backend.Player) {
        this.template = playerTemplate();
        super(this.template.$el);

        this.$el.find('.player-playlist')
        .replaceWith(new Playlist(audio.playlist).$el);

        this.setupDragEvents();

        this.uiTogglePlay =
            this.eventStream('click', '.player-control-play')
            .map(_ => {});

        this.uiToggleShowPlaylist =
            this.eventStream('click', '.player-toggle-playlist')
            .map(_ => {});

        this.uiToggleShowPlaylist.onValue(() => this.template.togglePlaylist());
        this.template.togglePlaylist(false);

        this.audio = audio;
        this.setupAudio();
    }

    private uiTogglePlay: Bacon.Stream<void>;
    private uiSetPosition: Bacon.Property<number>;
    private uiToggleShowPlaylist: Bacon.Stream<void>;

    private setupDragEvents() {
        var dragDropStream = dom.dragDropStream(
            this.$el.find('.player-drop-target')[0],
            'application/x-play-track'
        );

        dragDropStream.over.onValue(this.template.dropTarget)
        dragDropStream.drop
            .map(JSON.parse)
            .onValue( t => this.audio.play(t) )
    }

    private setupAudio() {
        var t = this.template;
        var audio = this.audio;

        audio.currentTrack.onValue(track => {
            t.empty(!track);
            if (track) {
                t.title(track.title);
                t.artist(track.artist);
            }
        });

        audio.playing.onValue(t.playing);
        audio.duration.onValue(t.duration);
        audio.playProgress.onValue(t.playProgress);
        audio.bufferProgress.onValue(t.bufferProgress);

        this.uiTogglePlay.onValue(() => audio.togglePlay());
    }

    private template: PlayerTemplate;
    private audio: Backend.Player;
}
export = Player;


class Playlist extends View {
    constructor(p: Backend.Playlist) {
        super($('<ol class=player-playlist>'));

        p.changes.onValue( s => {
            var nodes = splice.map(s, this.item);
            splice.applyNode(this.el, nodes);
        });
    }

    item(t: Track.Attributes): HTMLElement {
        return $('<li>').text(t.title)[0];
    }
}

/**
 * Template abstraction for the player
 *
 * Only concerns formatting and rendering data to the DOM.
 *
 * TODO Instead of progress in percent use absolute progress
 */
interface PlayerTemplate {
    $el: JQuery;
    duration(d: number);
    title(t: string);
    artist(a: string);
    playProgress(p: number);
    bufferProgress(p: number);
    empty(e: boolean);
    playing(p: boolean);
    dropTarget(e: boolean);
    togglePlaylist(show?: boolean);
}
function playerTemplate(): PlayerTemplate {

    var $el = $(templates.player());
    var $playlist = $el.find('.player-playlist-window');

    function durationFormat(d: number) {
        if (typeof d === 'undefined' || d === NaN)
            return '';
        else
            return moment.utc(d * 1000).format('m:ss');
    }

    return {
        $el: $el,

        empty:      dm.toggleClass('empty', $el),
        dropTarget: dm.toggleClass('over', $el),

        playing: dm.join(
            dm.attr('aria-label', $el, '.player-control-play')
              .trsf(p => p ? 'Pause' : 'Play')
          , dm.toggleClass('playing', $el)
        ),

        togglePlaylist(show?: boolean) {
            if (typeof show != 'boolean')
                show = $playlist.attr('aria-hidden') != undefined;

            if (show) {
                css.transitionToAutoHeight($playlist);
                $playlist.removeAttr('aria-hidden');
            } else {
                css.transitionFromAutoHeight($playlist, 0);
                $playlist.attr('aria-hidden', '');
            }
        },

        duration: dm.text($el, '.player-duration').trsf(durationFormat),

        title:  dm.text($el, '.player-title'),
        artist: dm.text($el, '.player-artist'),

        playProgress:   dm.attr('value', $el, '.player-progress-play'),
        bufferProgress: dm.attr('value', $el, '.player-progress-buffer')
    };
}
