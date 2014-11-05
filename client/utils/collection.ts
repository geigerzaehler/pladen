/// <reference path="../../typings/jquery/jquery.d.ts" />
/// <reference path="../../typings/underscore/underscore.d.ts" />
import _ = require('underscore');

/**
 * Inserting items into an array with indices.
 */


export interface Index<Item> {
    index: number;
    item:  Item;
}


/**
 * Insert 'items' into 'target' so that 'target' will still be ordered
 * by 'comparator'.
 *
 * Returns a list of indices that can be used to reply the insertion.
 *
 *    var copy = target.splice();
 *    var indices = insert(target, items, comparator);
 *    insertIndices(copy, indices);
 *    assertItemsEqual(copy, target);
 */
export function insert<Item>(
        target: Item[],
        items: Item[],
        comparator: (i: Item) => any
    ): Index<Item>[] {
    return items.map(function(item) {
        var i = _.sortedIndex(target, item, comparator);
        target.splice(i, 0, item);
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


export function mapIndices<T,S>(is: Index<T>[], it: (t: T) => S): Index<S>[] {
    return _.map(is, (i) => {
        return {
           index: i.index,
           item:  it(i.item)
        }
    })
}
