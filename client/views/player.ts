/// <reference path="../../typings/hammer.d.ts" />
import moment = require('moment');
import css = require('css_promise');
import hammer = require('hammer');

import TemplateView = require('./base/template');
import Track = require('models/track');

export = Player;

class Player extends TemplateView {

    get template() {
        return 'player';
    }

    get elementTemplate() {
        return '<div class="player empty">';
    }

    get events(): {[e: string]: string} {
        return {
            'click .player-control': 'togglePlay',
            'click .player-box': 'setPosition',
            'click .player-toggle-playlist': 'togglePlaylist'
        }
    }

    togglePlaylist(show?: boolean) {
        if (typeof show != 'boolean')
            show = this.$playlistWindow.attr('aria-hidden') != undefined;

        if (show) {
            css.transitionToAutoHeight(this.$playlistWindow);
            this.$playlistWindow.removeAttr('aria-hidden');
        } else {
            css.transitionFromAutoHeight(this.$playlistWindow, 0);
            this.$playlistWindow.attr('aria-hidden', '');
        }
        this.playlistOpen = show;
    }

    constructor() {
        super();
        this.setupAudio();
        this.render();
        // TODO caching fails
        this.$playlistWindow = this.$('.player-playlist-window');
        this.togglePlaylist(false);

        var h = hammer(this.el);
        h.get('swipe').set({direction: hammer.DIRECTION_VERTICAL})
        h.on('swipe', (e) => {
            if (this.playlistOpen && e.direction == hammer.DIRECTION_DOWN)
                this.togglePlaylist(false);
            else if (!this.playlistOpen && e.direction == hammer.DIRECTION_UP)
                this.togglePlaylist(true);
        })
    }

    play(track: Track.Attributes) {
        this.currentTrack = track;

        this.$el.removeClass('empty');
        this.audio.src = '/track/' + track.id + '/file';
        this.audio.play();

        this.$('.player-title').text(track.title);
        this.$('.player-artist').text(track.artist);

        this.playing = true;
        this.$el.toggleClass('playing', this.playing);
    }

    togglePlay(play: boolean) {
        this.playing = !this.playing
        this.$el.toggleClass('playing', this.playing);

        if (this.playing)
            this.audio.play();
        else
            this.audio.pause();
    }

    render() {
        super.render();
        this.progressPlay = this.$('.player-progress-play');
        this.progressBuffer = this.$('.player-progress-buffer');
        this.setupDragEvents();
    }

    private setPosition(e: any) {
        var x = e.clientX - this.$('.player-box').offset().left;
        var width = this.$('.player-box')[0].clientWidth;
        var time = x / width * this.duration;
        if (this.audio.buffered.length && this.audio.buffered.end(0) > time)
            this.audio.currentTime = time;
    }


    private setupAudio() {
        this.audio = new Audio();
        this.audio.preload = 'auto';
        this.audio.addEventListener('timeupdate', () => {
            this.updatePlayProgress();
        })
        this.audio.addEventListener('durationchange', () => {
            this.updateDuration();
        })
        this.audio.addEventListener('progress', () => {
            this.updateBufferProgress();
            this.updateDuration();
        })
        this.audio.addEventListener('ended', () => {
            this.playing = false;
            this.$el.toggleClass('playing', this.playing);
        })
    }

    private updatePlayProgress() {
        var part = this.audio.currentTime / this.duration;
        this.progressPlay.css('width', (part * 100) + '%');
    }

    private updateBufferProgress() {
        var part: number;
        if (this.audio.buffered.length == 0)
            part = 0;
        else
            part = this.audio.buffered.end(0) / this.duration;
        this.progressBuffer.css('width', (part * 100) + '%');
    }

    private updateDuration() {
        var duration = this.duration;
        if (!duration)
            return this.$('.player-duration').text('');

        var formatted = moment.utc(duration * 1000).format('m:ss');
        this.$('.player-duration').text(formatted);
        this.updatePlayProgress();
    }

    private get duration() {
        var duration = this.audio.duration;
        if (duration === Infinity || duration === NaN)
            duration = 0;
        if (!duration) {
            if (this.currentTrack)
                duration = this.currentTrack.length;
            else if (this.audio.buffered.length != 0)
                duration = this.audio.buffered.end(0);
        }
        return duration;
    }

    private setupDragEvents() {
        this.$('.player-box')
        .on('dragleave', (ev) => {
            this.$el.removeClass('over');
        })
        .on('dragenter', (ev) => {
            this.$el.addClass('over');
        })
        .on('dragover', (ev: any) => {
            ev.originalEvent.dataTransfer.effect = 'copy'
            ev.preventDefault();
        })
        .on('drop', (e: any) => {
            var dt = e.originalEvent.dataTransfer;
            var track: Track.Attributes;
            track = JSON.parse(dt.getData('application/x-play-track'));
            this.$el.removeClass('over');
            this.play(track)
        })
    }

    private currentTrack: Track.Attributes;
    private progressPlay: JQuery;
    private progressBuffer: JQuery;
    private audio: HTMLAudioElement;
    private playing: boolean = false;
    private $playlistWindow: JQuery;
    private playlistOpen: boolean;
}
