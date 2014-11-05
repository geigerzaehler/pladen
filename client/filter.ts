/// <reference path="../typings/underscore/underscore.d.ts" />
import _ = require('underscore');
import tr = require('tr');

import A = require('models/album');
import Album = A.Attributes;
import T = require('models/track');
import Track = T.Attributes;

var map = _.map;

/**
 * Create functions that match albums or tracks.
 *
 * The 'album' and 'track' functions take 'Filter' objects and return
 * 'Test' functions. If the test function is called on a track or
 * album object, it returns 'true' if the object matches the filter and
 * 'false' otherwise.
 *
 * TODO move this into 'models' module.
 */

export interface Filter {
    search?: string;
    downloadable?: boolean;
}

export interface Test<T> {
    (target: T): boolean;
}


export function album(f: Filter): Test<Album> {
    return releaseTester(['name', 'artist'], f);
}

export function track(f: Filter): Test<Track> {
    return releaseTester(['title', 'artist'], f);
}


function releaseTester(searchKeys: string[], f: Filter): Test<{}> {
    var tests: Test<{}>[] = [];
    if (f.search) {
        var stringTests = map(f.search.split(' '), function(s) {
            return trStringTester(new RegExp(s, 'i'));
        });
        tests.push(anyPropertyTester(searchKeys, stringTests));
    }
    if (isDefined(f.downloadable)) {
        tests.push(propertyTester('downloadable', boolTester(f.downloadable)));
    }

    return every(tests);
}


function every<T>(ts: Test<T>[]): Test<T> {
    return function everyTest(target: T) {
        return _.every(ts, (t) => t(target));
    }
}


function some<T>(ts: Test<T>[]): Test<T> {
    return function someTest(target: T) {
        return _.some(ts, (t) => t(target));
    }
}


function boolTester(expect: boolean): Test<any> {
    return function testBool(target: any) {
        return isDefined(target) && !!target == expect;
    }
}


function trStringTester(pattern: RegExp): Test<string> {
    return function testTrString(target: string) {
        return pattern.test(target) || pattern.test(tr(target));
    }
}


function propertyTester(key: string, test: Test<any>): Test<{}> {
    return function propertyTest(obj: {}) {
        return test(obj[key]);
    }
}


function anyPropertyTester(keys: string[], tests: Test<any>[]): Test<{}> {
    return function anyPropertyTest(obj: {}) {
        return _.every(tests, function(test) {
            return _.some(keys, key => test(obj[key]));
        });
    }
}


function isDefined(x) {
    return typeof x != 'undefined';
}
