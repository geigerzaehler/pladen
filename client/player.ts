/// <reference path="../typings/bacon.d.ts" />
import Bacon = require('bacon');

import splice = require('utils/splice');

import services = require('services');
import service = services.service;
import Service = services.Service;
import Track = require('models/track');


export var player: Service<services.Player> = service('player', () => new Player);
export class Player implements services.Player {

    playing: Bacon.Property<boolean>;
    currentTrack: Bacon.Property<Track.Attributes>;
    currentTime: Bacon.Property<number>;
    duration: Bacon.Property<number>;
    playProgress: Bacon.Property<number>;
    bufferProgress: Bacon.Property<number>;

    playlist: Playlist;

    play(track?: Track.Attributes) {
        if (track)
            this.playlist.set(track);
        this.backend.play();
    }

    queue(track: Track.Attributes) {
        this.playlist.queue(track);
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
        this.backend = new Audio();
        this.backend.preload = 'auto';
        this.playlist = new Playlist();

        this._currentTrack = new Bacon.Bus();
        this.currentTrack = this._currentTrack.toProperty(undefined);

        Bacon.fromEventTarget(this.backend, 'ended')
        .onValue(() => this.playlist.next());

        this.playlist.current.onValue(t => {
            if (t)
                this.backend.src = '/track/' + t.id + '/file';
            else
                this.backend.src = null;
            this._currentTrack.push(t);
        });

        // Current time and duration
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


        // Buffer length and progress
        var bufferLength =
            Bacon.fromEventTarget(this.backend, 'progress')
            .map(_ => this.backend.buffered.length && this.backend.buffered.end(0));
        
        this.bufferProgress = Bacon.update(
            NaN
          , [bufferLength, this.duration], (p, l, d) => l / d
        );


        // Playing and pausing
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

export class Playlist {

    changes: Bacon.Stream<splice.Splice<Track.Attributes>>;
    current: Bacon.Property<Track.Attributes>;

    constructor() {
        this.tracks = [];
        this._changes = new Bacon.Bus();
        this.changes = this._changes;
        this._current = new Bacon.Bus();
        this.current = this._current.toProperty();
    }

    queue(t: Track.Attributes) {
        if (!this.tracks.length && !this._current_track) {
            this.set(t);
        } else {
            this._changes.push(splice.insert(this.tracks.length, [t]))
            this.tracks.push(t);
        }
    }

    set(t: Track.Attributes) {
        this._current_track = t;
        this._current.push(t);
    }

    next() {
        this._changes.push(splice.remove(0, 1));
        this._current_track = this.tracks.shift();
        this._current.push(this._current_track);
    }

    private tracks: Array<Track.Attributes>;
    private _changes: Bacon.Bus<splice.Splice<Track.Attributes>>;
    private _current: Bacon.Bus<Track.Attributes>;
    private _current_track: Track.Attributes;
}

function snd<T>(x: any, y: T): T { return y }
