/// <reference path="../../typings/jquery/jquery.d.ts" />

export function find($root: JQuery, selector?: string) {
    if (typeof selector == 'undefined' || selector == '')
        return $root;
    else
        return $root.find(selector);
}


export function text($root: JQuery, selector?: string) {
    var $el = find($root, selector);
    return setter(function(t: any) {
        $el.text(t)
    });
}

export function toggleClass(klass: string, $root: JQuery, selector?: string) {
    var $el = find($root, selector);
    return function(b: boolean) {
        $el.toggleClass(klass, b);
    }
}

export function attr(name: string, $root: JQuery, selector?: string) {
    var $el = find($root, selector);
    return setter(function(value: any) {
        $el.attr(name, value);
    });
}

export function join<T>(f: (t:T) => void, g: (t:T) => void) {
    return function(t:T) {
        f(t);
        g(t);
    }
}

export interface Setter<T> {
    (t: T): void;
    trsf<S>(f: (s: S) => T): Setter<S>;
}

function setter<T>(f: (t: T) => void): Setter<T> {
    var s = <Setter<T>>f;
    s.trsf = function<S>(g: (s: S) => T) {
        return setter(function(s:S) {
            f(g(s));
        })
    }
    return s;
}
