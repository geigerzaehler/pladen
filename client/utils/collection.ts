/// <reference path="../../typings/all.d.ts" />

import _ = require('underscore');


export function insert<Item>(
        into: Item[],
        items: Item[],
        comparator: (i: Item) => any
    ): Index<Item>[] {
    return _.map(items, (item) => {
        var i = _.sortedIndex(into, item, comparator);
        into.splice(i, 0, item);
        return { index: i, item:  item };
    });
}

export function insertIndices<Item>(into: Item[], is: Index<Item>[]) {
    _.each(is, (i) => {
        into.splice(i.index, 0, i.item);
    });
}

export function insertDomIndices(parent: JQuery, is: Index<JQuery>[]) {
    _.each(is, (i) => {
        var before = parent.children().eq(i.index);
        if (before.length)
            before.before(i.item);
        else
            parent.append(i.item);
    });
}

export interface Index<Item> {
    index: number;
    item:  Item;
}

export function mapIndices<T,S>(is: Index<T>[], it: (t: T) => S): Index<S>[] {
    return _.map(is, (i) => {
        return {
           index: i.index,
           item:  it(i.item)
        }
    })
}
