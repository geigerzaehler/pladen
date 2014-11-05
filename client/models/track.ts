/// <reference path="../../typings/tsd.d.ts" />
/// <reference path="../../typings/signals.d.ts" />
/// <reference path="../../typings/superagent.d.ts" />

import r = require('request');
import Request = r.Request;
import request = r.create;
import Response = r.Response;

import when = require('when');
import Promise = when.Promise;

import underscore = require('underscore');
var values = underscore.values;
var map = underscore.map;
var filter = underscore.filter;
var sort = underscore.sortBy;
var sortedIndex = underscore.sortedIndex;
var each = underscore.each;

import moment = require('moment');
import Signal = require('signals');
import SignalObserver = require('signal_observer');


export class Repository implements ICollection {

    added: Signal<Track[]> = new Signal<Track[]>();
    removed: Signal<Track[]> = new Signal<Track[]>();

    fetchAll(): Promise<void> {
        return this.fetchCollection().yield(null);
    }

    fetchCollection(path?: string): Promise<Track[]> {
        return this
        .request
        .get(path)
        .run()
        .with(this)
        .then(this.parse)
        .then(this.merge)
    }

    singletons(): SingletonCollection {
        return new SingletonCollection(this);
    }

    get(id: number) {
        return this.models[id];
    }


    private parse(res: Response): Attributes[] {
        var attrs = res.body && res.body['tracks'];
        if (!attrs.forEach)
            return [];
        return underscore.map(attrs, (a: any) => {
            if (a.added)
                a.added = moment.unix(a.added);
            return pickAttributes(a);
        })
    }

    private merge(attrs: Attributes[]) {
        var added: Track[] = [];
        var merged: Track[] = [];
        attrs.forEach((attr) => {
            var id = attr.id;
            if (!id) return;

            var existing = this.models[id]
            if (existing) {
                existing.update(attr)
            } else {
                existing = new Track(attr);
                this.models[id] = existing;
                added.push(existing);
            }
            merged.push(existing)
        });
        this.added.dispatch(added);
        return merged;
    }

    private url: string = "/track";
    private models: {[id: number]: Track} = {};

    private request: Request = request()
                               .get(this.url)
                               .accept('application/json');
}

// TODO better name
export interface ICollection {
    added: Signal<Track[]>;
    removed: Signal<Track[]>;

    fetchAll(): Promise<void>;
}


export class SingletonCollection implements ICollection {

    added: Signal<Track[]> = new Signal<Track[]>();
    removed: Signal<Track[]> = new Signal<Track[]>();

    constructor(private repo: Repository) {
        repo.added.add((tracks) => {
            tracks = filter(tracks, this.predicate)
            this.models = this.models.concat(tracks)
            this.added.dispatch(tracks);
        })
    }

    fetchAll() {
        return this.repo.fetchCollection(this.path).yield(null);
    }

    recent() {
        var recent_ = moment().subtract(60, 'days');
        function predicate(track: Track) {
            return track.added.isAfter(recent_);
        }
        function comparator(track: Track) {
            return -track.added.unix();
        }
        return new OrderedCollection(this, this.models, comparator, predicate);
    }

    private predicate(track: Track) {
        return track.albumid === null || track.albumid === undefined;
    }

    private path = 'singleton';
    private models: Track[] = [];
}


export class OrderedCollection {

    inserted = new Signal<Index>();
    removed = new Signal<Index>();

    constructor(
        private backend: ICollection,
        private items: Track[],
        private comparator?: Comparator,
        private predicate?: Predicate
    ) {
        this.repoObserver = new SignalObserver(this);
        this.repoObserver.on(backend.added, this.repoAdded)
        this.repoAdded(items);
    }


    private repoAdded(items: Track[]) {
        if (this.predicate) {
            items = filter(items, this.predicate)
        }
        each(items, (item) => {
            var i = this.items.length;
            if (this.comparator)
                i = sortedIndex(this.items, item, this.comparator);
            this.items.splice(i, 0, item);
            this.inserted.dispatch({
                index: i,
                item:  item
            });
        });
    }

    private repoObserver: SignalObserver;
}


export interface Index {
    index: number;
    item: Track;
}

export interface Predicate {
    (album: Track): boolean;
}

export interface Comparator {
    (album: Track): any;
}

export interface Attributes {
    title:    string;
    no?:      number;
    artist:   string;
    length:   number;
    added:    Moment;
    downloadable: boolean;

    id:       number;
    albumid?: number;
}

export function pickAttributes(obj: {}): Attributes {
    return underscore.pick(obj, 'id', 'albumid', 'title', 'no', 'artist'
                              , 'length', 'added', 'downloadable');
}

export function extendAttributes(target: Attributes, obj: {}) {
    underscore.extend(target, pickAttributes(obj));
}


export class Track implements Attributes {
    constructor(a: Attributes) {
        this.update(a);
    }

    update(attr: Attributes) {
        extendAttributes(this, attr);
    }

    get fileUrl() {
        return '/track/' + this.id + '/file'
    }

    title:    string;
    no:       number;
    artist:   string;
    albumid:  number;
    id:       number;
    length:   number;
    added:    Moment;
    downloadable: boolean;
}
