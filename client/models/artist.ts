import Album = require('./album');
import Track = require('./track');

import tr = require('tr');
import Signal = require('signals');
import underscore = require('underscore');
var sort = underscore.sortBy;

export class Collection {
    constructor(private albums: Album.Repository,
                private tracks: Track.SingletonCollection) {
        tracks.added.add((ts) => {
            underscore.each(groupByArtist(ts), (tracks, name) => {
                this.getArtist(name).addTracks(tracks);
            });
        })
        albums.added.add((as) => {
            underscore.each(groupByArtist(as), (albums, name) => {
                this.getArtist(name).addAlbums(albums);
            });
        })
    }

    inserted = new Signal<Index>();

    each(it: (artist: Model) => any) {
        underscore.each(this.artists, it);
    }

    private getArtist(name: string): Model {
        var found = this.nameMap[name];
        if (found) return found;

        var artist = new Model(name);
        var at = underscore.sortedIndex(this.artists, artist, comparator);
        this.artists.splice(at, 0, artist);
        this.nameMap[name] = artist;
        this.inserted.dispatch({index: at, model: artist});
        return artist;
    }

    private artists: Model[] = [];
    private nameMap: {[name: string]: Model} = {};
}

function groupByArtist<T extends {artist: string}>(xs: T[]): {[name: string]: T[]} {
    return underscore.groupBy(xs, 'artist');
}

function comparator(artist: Model) {
  return tr(artist.name)
         .replace(/^The /, '')
         .replace(/^A /, '')
}

export interface Index {
    index: number;
    model: Model;
}

export class Model {
    name:   string;
    albums: Album.Album[] = [];
    tracks: Track.Track[] = [];

    changed = new Signal<void>();

    constructor(name: string) {
        this.name = name;
    }

    addTracks(ts: Track.Track[]) {
        this.tracks = this.tracks.concat(ts);
        this.changed.dispatch(null);
    }

    addAlbums(as: Album.Album[]) {
        this.albums = sort(this.albums.concat(as), (a) => {
            return -a.year;
        });
        this.changed.dispatch(null);
    }
}
