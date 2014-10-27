/// <reference path="../../typings/bacon.d.ts" />
import Bacon = require('bacon')
import moment = require('moment');

import css = require('css_promise');
import dom = require('dom');
import templates = require('templates');

import ViewModule = require('views/base/view2');
import View = ViewModule.View;

import Track = require('models/track');


export class Player extends View {
    constructor() {
        this.template = playerTemplate();
        super(this.template.$el);

        this.state = { playing: false }

        this.setupDragEvents();

        this.uiTogglePlay =
            this.eventStream('click', '.player-control-play')
            .map(_ => {});

        this.uiToggleShowPlaylist =
            this.eventStream('click', '.player-toggle-playlist')
            .map(_ => {});

        this.uiToggleShowPlaylist.onValue(() => this.template.togglePlaylist());
        this.template.togglePlaylist(false);

        this.setupAudio();
    }

    private uiTogglePlay: Bacon.Stream<void>;
    private uiSetPosition: Bacon.Property<number>;
    private uiToggleShowPlaylist: Bacon.Stream<void>;

    private state: PlayerState;


    play(track: Track.Attributes) {
        this.audio.play(track);
    }

    private setupDragEvents() {
        var dragDropStream = dom.dragDropStream(this.$('.player-drop-target')[0],
                                                'application/x-play-track');

        dragDropStream.over.onValue(this.template.dropTarget)
        dragDropStream.drop
            .map(JSON.parse)
            .onValue( t => this.play(t) )
    }

    private setupAudio() {
        var t = this.template;
        var audio = this.audio = new PlayerAudio();

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

    private audio: PlayerAudio;
    private template: PlayerTemplate;
}


/**
 * Template abstraction for the player
 *
 * Only concerns formatting and rendering data to the DOM.
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
    var $duration = $el.find('.player-duration');
    var $title = $el.find('.player-title');
    var $artist = $el.find('.player-artist');
    var $duration = $el.find('.player-duration');
    var $playProgress = $el.find('.player-progress-play');
    var $bufferProgress = $el.find('.player-progress-buffer');
    var $playlist = $el.find('.player-playlist-window');
    var $playPause = $el.find('.player-control-play');

    return {
        $el: $el,

        empty: function(e: boolean) {
            $el.toggleClass('empty', e);
        },

        dropTarget: function(d: boolean) {
            $el.toggleClass('over', d);
        },

        playing: function(p: boolean) {
            var label = p ? 'Pause' : 'Play'
            $playPause.attr('aria-label', label);
            $el.toggleClass('playing', p);
        },

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

        duration: function(d: number) {
            var formatted;
            if (typeof d === 'undefined' || d === NaN)
                formatted = '';
            else
                formatted = moment.utc(d * 1000).format('m:ss');
            $duration.text(formatted);
        },

        title: function(t: string) {
            $title.text(t);
        },

        artist: function(a: string) {
            $artist.text(a);
        },

        playProgress: function(p: number) {
            $playProgress.css('width', (p*100) + '%');
        },

        bufferProgress: function(p: number) {
            $bufferProgress.css('width', (p*100) + '%');
        }
    };
}


interface PlayerState {
    playing: boolean;
    currentTrack?: Track.Track;
}


/**
 * Adapter for the HTMLAudioElement that is used by Player
 */
class PlayerAudio {

    playing: Bacon.Property<boolean>;
    currentTrack: Bacon.Property<Track.Attributes>;
    currentTime: Bacon.Property<number>;
    duration: Bacon.Property<number>;
    playProgress: Bacon.Property<number>;
    bufferProgress: Bacon.Property<number>;

    play(track: Track.Attributes) {
        this._currentTrack.push(track);
        this.backend.src = '/track/' + track.id + '/file';
        this.backend.play();
    }
    
    pause() {
        this.backend.pause();
    }

    togglePlay() {
        if (this._isPlaying)
            this.backend.pause();
        else
            this.backend.play();
    }


    constructor() {
        var w: any = window;
        w._a = this;
        this.backend = new Audio();
        this.backend.preload = 'auto';


        this._currentTrack = new Bacon.Bus();
        this.currentTrack = this._currentTrack.toProperty(undefined);


        this.currentTime =
            Bacon.fromEventTarget(this.backend, 'timeupdate')
            .map(_ => this.backend.currentTime)
            .toProperty(this.backend.currentTime);

        var currentTrackLength =
            this.currentTrack
            .map(t => t && t.length)

        var backendDuration =
            Bacon.fromEventTarget(this.backend, 'durationchange')
            .map(_ => this.backend.duration)
            .filter(l => l !== Infinity)
            .toProperty(this.backend.duration);

        this.duration = Bacon.update(
            NaN
          , [currentTrackLength.toEventStream()], snd
          , [backendDuration.toEventStream()], snd
        );

        this.playProgress = Bacon.combineWith(
            (currentTime, duration) => currentTime / duration,
            this.currentTime, this.duration
        );


        var bufferLength =
            Bacon.fromEventTarget(this.backend, 'progress')
            .map(_ => this.backend.buffered.end(0));
        
        this.bufferProgress = Bacon.update(
            NaN
          , [bufferLength, this.duration], (p, l, d) => l / d
        );


        var playingEvent = Bacon.fromEventTarget(this.backend, 'playing');
        var pauseEvent = Bacon.fromEventTarget(this.backend, 'pause');

        this.playing = Bacon.update(
            false
          , [playingEvent], () => true
          , [pauseEvent], () => false
        );
        this.playing.onValue( x => this._isPlaying = x );

    }


    private backend: HTMLAudioElement;
    private _currentTrack: Bacon.Bus<Track.Attributes>;
    private _isPlaying: boolean;
}


function snd<T>(x: any, y: T): T { return y }
