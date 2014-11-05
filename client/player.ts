/// <reference path="../typings/bacon.d.ts" />
import Bacon = require('bacon');

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
        this.backend = new Audio();
        this.backend.preload = 'auto';

        this._currentTrack = new Bacon.Bus();
        this.currentTrack = this._currentTrack.toProperty(undefined);

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


function snd<T>(x: any, y: T): T { return y }
