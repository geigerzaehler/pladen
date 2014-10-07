/// <reference path="../../typings/all.d.ts" />

import underscore = require('underscore');
var map = underscore.map;
var filter = underscore.filter;

import c = require('utils/collection');
import Signal = require('signals');
import moment = require('moment');

import A = require('./album');
import T = require('./track');
import B = require('./base');


/**
 * Sum type of Album and Track
 */
export class Release {
    album: A.Album;
    track: T.Track;

    constructor(album: A.Album);
    constructor(track: T.Track);
    constructor(release) {
        if (release instanceof T.Track)
            this.track = release;
        else if (release instanceof A.Album)
            this.album = release;
        else
            throw new Error(release + 'should be Album or Track');
    }

    get added(): Moment {
        if (this.album)
            return this.album.added;
        else
            return this.track.added;

    }
}

export class Recent implements B.List<Release> {
    inserted = new Signal<Index[]>();
    deleted  = new Signal<Index[]>();
    added    = new Signal<Release[]>();
    removed  = new Signal<Release[]>();
    items    = [];

    constructor(tracks: B.Set<T.Track>, albums: B.Set<A.Album>) {
        this.add(tracks.items);
        this.add(albums.items);

        tracks.added.add((ts) => { this.add(ts) });
        albums.added.add((as) => { this.add(as) });

        this.inserted.add((rs) => { this.added.dispatch(B.items(rs)) });
        this.deleted.add((rs) =>  { this.removed.dispatch(B.items(rs)) });
    }

    private add(tracks: T.Track[]);
    private add(albums: A.Album[]);
    private add(rs) {
        var releases = map(rs, (r:any) => { return new Release(r) });
        releases = filter(releases, this.predicate, this);
        var is = c.insert(this.items, releases, this.comparator);
        this.inserted.dispatch(is);
    }

    private comparator(r: Release): number {
        return -r.added.unix();
    }

    private predicate(r: Release) {
        return r.added.isAfter(this._recent);
    }

    private _recent = moment().subtract(60, 'days');
}

export interface Index extends B.Index<Release> {}
