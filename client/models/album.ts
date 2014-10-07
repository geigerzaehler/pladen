/// <reference path="../../typings/all.d.ts" />

import r = require('request');
import request = r.create;
import Request = r.Request;
import Response = r.Response;

import when = require('when');
import Promise = when.Promise;

import underscore = require('underscore');
var values = underscore.values;
var each = underscore.each;
var map = underscore.map;
var extend = underscore.extend;
var find = underscore.find;
var filter = underscore.filter;
var sort = underscore.sortBy;
var sortedIndex = underscore.sortedIndex;

import Signal = require('signals');
import SignalObserver = require('signal_observer');
import w = require('when');
import moment = require('moment');
import B = require('./base')


export class Repository implements B.Repo<Album>{

    fetchAll(): Promise<void> {
        return this
        .request
        .run()
        .with(this)
        .then(this.parse)
        .then(this.merge)
        .yield(null);
    }

    fetch(id: number): Promise<Album> {
        return this
        .request
        .get('' + id)
        .run()
        .then(this.parse)
        .then((attrs: Attributes[]) => {
            return underscore.find(attrs, (a) => { return a.id == id });
        }).then((attr: Attributes) => {
            if (!attr)
                throw new Error("response does not contain albums");
            return this.merge([attr])[0];
        });
    }

    collection(): Collection {
        return new Collection(this, this.items);
    }

    recent(): Collection {
        var recent_ = moment().subtract(60, 'days');
        function predicate(album: Album) {
            return album.added.isAfter(recent_);
        }
        function comparator(album: Album) {
            return -album.added.unix();
        }
        return new Collection( this, this.items
                             , comparator, predicate);
    }

    added: Signal<Album[]> = new Signal<Album[]>();
    removed: Signal<Album[]> = new Signal<Album[]>();

    private parse(res: Response): Attributes[] {
        var attrs = res.body && res.body['albums'];
        if (!attrs.forEach)
            return [];
        return underscore.map(attrs, (a: any) => {
            if (a.added)
                a.added = moment.unix(a.added);
            return pickAttributes(a);
        })
    }

    private merge(attrs: Attributes[]) {
        var added: Album[] = [];
        var merged: Album[] = [];
        attrs.forEach((attr) => {
            var id = attr.id;
            if (!id) return;

            var existing = this.itemsById[id]
            if (existing) {
                existing.update(attr)
            } else {
                existing = new Album(this, attr);
                this.itemsById[id] = existing;
                this.items.push(existing)
                added.push(existing);
            }
            merged.push(existing)
        });
        this.added.dispatch(added);
        return merged;
    }

    public  url: string = "/album";
    private itemsById: {[id: number]: Album} = {};
    public  items:     Album[] = [];

    private request: Request = request()
                               .get(this.url)
                               .accept('application/json');
}

export class Collection {

    inserted = new Signal<Index>();
    removed = new Signal<Index>();

    each(fn: (album: Album) => any) {
        underscore.each(this.items, fn);
    }

    constructor(
        private repo: Repository,
        private items: Album[],
        private comparator?: Comparator,
        private predicate?: Predicate
    ) {
        this.repoObserver = new SignalObserver(this);
        this.repoObserver.on(repo.added, this.repoAdded)
        this.repoAdded(items);
    }


    private repoAdded(items: Album[]) {
        if (this.predicate) {
            items = filter(items, (item) => {
                return this.predicate(item)
            });
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
    item: Album;
}

export interface Predicate {
    (album: Album): boolean;
}

export interface Comparator {
    (album: Album): any;
}

export interface Attributes {
    name?: string;
    artist?: string;
    year?: number;
    added?: Moment;
    tracks?: Array<any>;
    downloadable?: boolean
    id?: number;
}

function pickAttributes(obj: {}): Attributes {
    return underscore.pick(obj, 'id', 'name', 'artist',
            'year', 'added', 'tracks', 'downloadable');
}

function extendAttributes(target: Attributes, obj: {}) {
    underscore.extend(target, pickAttributes(obj));
}


export class Album implements Attributes {
    name: string;
    artist: string;
    year: number;
    added: Moment;
    tracks: Array<any>;
    downloadable: boolean
    id: number;

    changed = new Signal<string>();

    url(path?: string) {
        var url = this.repo.url + '/' + this.id;
        if (path)
            url = url + '/' + path;
        return url;
    }

    request() {
        return request(this.url());
    }

    constructor(private repo: Repository, a: Attributes) {
        this.update(a);
    }

    update(a: Attributes) {
        extendAttributes(this, a);
        this.changed.dispatch();
    }

    fetch(): Promise<void> {
        return this.repo.fetch(this.id).then(() => {})
    }

    /**
     * Tries to open the URL for the album’s tar file.
     *
     * Rejects the promise if server response is not ok
     */
    download(): Promise<void> {
        if (!this.downloadable)
            return when.reject(new Error("Album is not downloadable"))
        return request()
        .head(this.downloadURL)
        .run()
        .then((res) => {
            document.location.assign(this.downloadURL);
        });
    }

    requestUpload(): Promise<Response> {
        return this.request().put('request').run()
    }

    get downloadURL():string {
        return this.url('tar');
    }
}
