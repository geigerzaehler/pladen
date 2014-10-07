/// <reference path="../../typings/all.d.ts" />

import underscore = require('underscore');
import Signal = require('signals');
import when = require('when');
import Promise = when.Promise;

export interface Repo<Model> extends Set<Model> {
    fetchAll(): Promise<void>;
    fetch(id: number): Promise<Model>;
}

export interface Set<Model> {
    added:   Signal<Model[]>;
    removed: Signal<Model[]>;
    items:   Model[];
}

export interface List<Model> extends Set<Model> {
    inserted: Signal<Index<Model>[]>;
    deleted:  Signal<Index<Model>[]>;
}

export interface Index<Model> {
    index: number;
    item:  Model;
}

export function items<Model>(is: Index<Model>[]): Model[] {
    return underscore.map((is), (i) => { return i.item })
}
