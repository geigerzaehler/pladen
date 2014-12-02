/// <reference path="../../typings/jquery/jquery.d.ts" />

export interface Splice<T> {
    items: Array<T>;
    index: number;
    remove: number;
}

export function insert<T>(index: number, items: Array<T>): Splice<T> {
    return {
        items: items,
        index: index,
        remove: 0
    };
}

export function remove(index: number, remove: number): Splice<any> {
    return {
        items: [],
        index: index,
        remove: remove
    };
}

export function apply<T>(target: Array<T>, splice: Splice<T>) {
    var args: any[] = [splice.index, splice.remove];
    args = args.concat(splice.items);
    target.splice.apply(target, args);
    return target;
}

export function map<T,S>(splice: Splice<T>, f: (t:T) => S): Splice<S> {
    return {
        items: splice.items.map(f),
        index: splice.index,
        remove: splice.remove
    };
}

function slice(target: any, start?: number, end?: number): Array<any> {
    return Array.prototype.slice.call(target, start, end);
}

export function applyNode(target: Node, splice: Splice<Node>) {
    var remove: Node[] = slice(target.childNodes, splice.index,
                               splice.index + splice.remove);
    var before: Node = target.childNodes[splice.index];
    if (before)
        splice.items.forEach(n => target.insertBefore(n, before));
    else
        splice.items.forEach(n => target.appendChild(n));
    remove.forEach(n => target.removeChild(n));
    return target;
}
